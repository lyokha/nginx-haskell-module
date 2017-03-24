{-# LANGUAGE TemplateHaskell, DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms, TupleSections, NumDecimals #-}

{- ghc -O2 -dynamic -shared -fPIC -lHSrts_thr-ghc$(ghc --numeric-version) \
 - lmr.hs -o /var/lib/nginx/lmr.so -fforce-recomp
 -
 - for making eventlog:
 - ghc -O2 -dynamic -shared -fPIC \
 - -lHSrts_thr_debug-ghc$(ghc --numeric-version) \
 - lmr.hs -o /var/lib/nginx/lmr.so -fforce-recomp -eventlog
 -
 - and put in nginx.conf
 - haskell rts_options -l; -}

module NgxHaskellUserRuntime where

import           NgxExport
import           Data.List
import           Data.List.NonEmpty (nonEmpty, NonEmpty ((:|)))
import           Data.Function
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Time.Clock
import           Network.HTTP.Client
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Arrow
import           System.IO.Unsafe
import           Data.IORef
import           Data.RoundRobin
import           Data.Maybe
import           Data.Aeson
import           GHC.Generics
import           Safe

pattern GroupFailed = (0, 0, Nothing)

type Label = String
type Hint = String
type Destination = String   -- IP address or domain name
type Url = String           -- normally must start with /

data Op = Read
        | Write
        deriving (Read, Show)

data Possession = Own
                | Remote Destination
                deriving (Generic, Eq, Ord)
instance ToJSON Possession
instance ToJSONKey Possession

data Mode = RW
          | RO
          deriving (Generic, Eq, Ord)
instance FromJSON Mode
instance ToJSON Mode

type Backends = Map Possession [Destination]
type Routes = Map Hint (Map Label Backends)
type RRBackends = Map Possession ([Destination], Maybe (RoundRobin Int))
type RRRoutes = Map Hint (Map Label RRBackends)
type SeqNumber = Integer
type Timestamp = Integer

data BackendStatus = Ok             -- In / Out
                   | NotFound       -- In
                   | NotAccessible  -- In
                   | NotReadable    -- In
                   | NonExistent    -- Out
                   deriving (Read, Show, Eq)

data Msg = Msg { op      :: Op
               , hnt     :: Hint
               , label   :: Label
               , seqn    :: SeqNumber
               , key     :: Int
               , start   :: Int
               , idx     :: Int
               , backend :: Destination
               , status  :: BackendStatus
               } deriving (Read, Show)

data TimeInterval = Hr Int
                  | Min Int
                  | Sec Int
                  | HrMin (Int, Int)
                  | MinSec (Int, Int)
                  deriving Read

data Conf = Conf { updateInterval    :: TimeInterval
                 , blacklistInterval :: TimeInterval
                 , backends          :: (Url, [Destination])
                 , partners          :: (Url, [Destination])
                 } deriving Read

data BackendData = BackendData { timestamp :: Timestamp
                               , labels    :: Map Label LabelData
                               } deriving Generic
instance FromJSON BackendData
instance ToJSON BackendData

type PartnerData = Map Destination BackendData
type CollectedData = Map Possession PartnerData

data LabelData = LabelData { mode :: Mode
                           , hint :: [Hint]
                           } deriving Generic
instance ToJSON LabelData
instance FromJSON LabelData

data TaggedHint = AnyHint
                | PlainHint Hint
                deriving Eq
instance Show TaggedHint where
    show AnyHint       = "any"
    show (PlainHint x) = x

allBackends :: IORef CollectedData
allBackends = unsafePerformIO $ newIORef M.empty
{-# NOINLINE allBackends #-}

routes :: IORef ((SeqNumber, (RRRoutes, RRRoutes)),
                 (SeqNumber, (RRRoutes, RRRoutes)))
routes = unsafePerformIO $ newIORef ((-1, (M.empty, M.empty)),
                                     (0,  (M.empty, M.empty)))
{-# NOINLINE routes #-}

blacklist :: IORef (Map Destination UTCTime)
blacklist = unsafePerformIO $ newIORef M.empty
{-# NOINLINE blacklist #-}

blInterval :: IORef Int
blInterval = unsafePerformIO $ newIORef 60
{-# NOINLINE blInterval #-}

getResponse url = fmap responseBody . (parseRequest url >>=)

httpManager = unsafePerformIO $ newManager defaultManagerSettings
{-# NOINLINE httpManager #-}

getUrl url = getResponse url $ flip httpLbs httpManager

both :: Arrow a => a b c -> a (b, b) (c, c)
both = join (***)

bothKleisli :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
bothKleisli = runKleisli . both . Kleisli

toRoutes :: CollectedData -> (Routes, Routes)
toRoutes = both (mkRoutes . sort) .
    (
        map (first show) . filter ((AnyHint ==) . fst) . map snd &&&
        map (show *** first (const "any")) . filter ((AnyHint /=) . fst) .
            fromMaybe [] . M.lookup RW . combineByFst .
                sortBy (compare `on` fst)
    ) .
    concatMap (\(m, hs, l, p, d) -> [(m, (h, (l, (p, d)))) | h <- hs]) .
        concatMap (\(p, v) ->
            concatMap (\(d, v) ->
                map (\(l, v) -> (mode v, ckHint $ hint v, l, p, d)) $
                    M.assocs $ labels v) $ M.assocs v) . M.assocs
    where ckHint h = AnyHint :
              if null h then [PlainHint "default"] else map PlainHint h
          combineByFst :: Ord a => [(a, b)] -> Map a [b]
          combineByFst =
              M.fromDistinctAscList .
                  map (uncurry (foldr $ \b (c, a) -> (c, snd b : a)) .
                        ((, []) . fst . head &&& id)) . groupBy ((==) `on` fst)
          mkRoutes =
              M.map (M.map $ M.map (map head . group . sort) . combineByFst) .
                  M.map combineByFst . combineByFst

mkRRRoutes :: (Routes, Routes) -> IO (RRRoutes, RRRoutes)
mkRRRoutes = bothKleisli $ mapM $ mapM $ mapM $
    runKleisli $
        arr id &&&
        Kleisli (
                    maybe (return Nothing) (fmap Just) .
                        (nonEmpty . flip take [1 ..] . length >=> ckLen >=>
                            Just . newRoundRobin)
                )
    where ckLen x@(_ :| _ : _) = Just x
          ckLen _ = Nothing

fromRRRoutes :: (RRRoutes, RRRoutes) -> (Routes, Routes)
fromRRRoutes = both $ M.map $ M.map $ M.map fst

queryEndpoints (C8.unpack -> conf) firstRun = do
    let Conf (toSec -> upd) (toSec -> bl) (url, own) (purl, remote) =
            readDef (Conf (Hr 24) (Min 1) ("", []) ("", [])) conf
    if firstRun
        then atomicWriteIORef blInterval bl
        else threadDelaySec upd
    (M.fromList -> obd) <-
        mapConcurrently
            (\d -> catchBadResponseOwn d $
                second (fromMaybe (BackendData 0 M.empty) . decode) <$>
                    query url d
            ) own
    (M.fromList . ((Own, obd) :) . map (first Remote) -> abd) <-
        mapConcurrently
            (\d -> catchBadResponseRemote d $
                second (fromMaybe M.empty . decode) <$>
                    query purl d
            ) remote
    oldbd <- readIORef allBackends
    let allbd =
            M.mapWithKey
                (\p -> M.mapWithKey
                    (\d a -> case M.lookup p oldbd >>= M.lookup d of
                        Just v@(BackendData ts _) ->
                            if ts > timestamp a then v else a
                        _ -> a
                    )
                ) abd
        newRoutes = toRoutes allbd
    atomicWriteIORef allBackends allbd
    oldRoutes <- fromRRRoutes . snd . snd <$> readIORef routes
    if newRoutes == oldRoutes && not firstRun
        then return C8L.empty
        else do
            rr <- mkRRRoutes newRoutes
            atomicModifyIORef' routes $ \((a, _), o) -> ((o, (a + 2, rr)), ())
            return $ encode newRoutes
    where query u = runKleisli $ arr id &&& Kleisli (getUrl . flip mkAddr u)
          mkAddr = (("http://" ++) .) . (++)
          catchBadResponse f d = handle $
              \(_ :: SomeException) -> ((d, ) . f d) <$> readIORef allBackends
          catchBadResponseOwn = catchBadResponse $
              \d -> fromMaybe (BackendData 0 M.empty) . M.lookup d .
                  fromMaybe M.empty . M.lookup Own
          catchBadResponseRemote = catchBadResponse $
              \d -> fromMaybe M.empty . M.lookup (Remote d)
          threadDelaySec = threadDelay . (* 1e6)
          toSec (Hr h)          = 3600 * h
          toSec (Min m)         = 60 * m
          toSec (Sec s)         = s
          toSec (HrMin (h, m))  = 3600 * h + 60 * m
          toSec (MinSec (m, s)) = 60 * m + s
ngxExportServiceIOYY 'queryEndpoints

readMsg = readDef (Msg Read "" "" 0 0 0 0 "" NotReadable) . C8.unpack

writeMsg = return . C8L.pack . show

writeFinalMsg m = writeMsg m { backend = "STOP", status = NonExistent }

getMsg (readMsg -> m@Msg { status = NotReadable }) =
    writeFinalMsg m
getMsg (readMsg -> m@(Msg op hnt label seqn key start idx b st)) = do
    when (st == NotAccessible) $ do
        bl <- readIORef blacklist
        when (b `M.notMember` bl) $
            getCurrentTime >>= modifyIORef' blacklist . M.insert b
    (getRoutes seqn >=> return . rSelect op >=>
        return . second (M.lookup hnt >=> M.lookup label) -> r) <-
            readIORef routes
    case r >>= \x@(_, d) -> (x, ) <$> (d >>= elemAt key) of
        Nothing -> writeFinalMsg m
        Just ((n, fromJust -> d), (_, gr)) -> do
            (s, i, b) <- if st == NotFound
                             then return GroupFailed
                             else getNextInGroup start
                                      (advanceIdx start st idx) gr
            case b of
                Nothing -> do
                    ((s, i, b), k) <- getNext (key + 1) d
                    case b of
                        Nothing -> do
                            unblacklistAll d
                            writeFinalMsg m { seqn = n }
                        Just v -> writeMsg $ Msg op hnt label n k s i v Ok
                Just v -> writeMsg $ Msg op hnt label n key s i v Ok
    where rSelect Read  = second fst
          rSelect Write = second snd
          getRoutes v (a@(x, _), b@(y, _)) | v == 0 = Just b
                                           | v == y = Just b
                                           | v == x = Just a
                                           | otherwise = Nothing
          elemAt i m | i < M.size m = Just $ M.elemAt i m
                     | otherwise = Nothing
          getNextInGroup _ 1 (_, Nothing) =
              return GroupFailed
          getNextInGroup _ _ (dst, Nothing) =
              (0, 0, ) <$> ckBl (head dst)  -- using head is safe here
                                            -- because dst cannot be []
          getNextInGroup s i (dst, Just rr) = do
              ns <- if s == 0 then select rr else return s
              ((i +) . length -> ni, headDef Nothing -> d) <-
                  span isNothing <$> mapM ckBl
                        (take (length dst - i) $ drop (ns - 1 + i) $ cycle dst)
              return (ns, ni, d)            -- group fails when d == Nothing
          advanceIdx 0 Ok = const 0
          advanceIdx 0 _  = const 1
          advanceIdx _ _  = succ
          ckBl d = do
              bl <- readIORef blacklist
              case M.lookup d bl of
                  Nothing -> return $ Just d
                  Just t -> do
                      now <- getCurrentTime
                      (fromIntegral -> bli) <- readIORef blInterval
                      if diffUTCTime now t > bli
                          then do
                              modifyIORef' blacklist $ M.delete d
                              return $ Just d
                          else return Nothing
          getNext k d = do
              (length -> nk, headDef GroupFailed -> d) <-
                  span (\(_, _, b) -> isNothing b) <$>
                      mapM (getNextInGroup 0 0) (M.elems $ M.drop k d)
              return (d, k + nk)
          unblacklistAll = mapM_ $
              mapM_ (modifyIORef' blacklist . M.delete) . fst
ngxExportIOYY 'getMsg

getOwnBackends = const $
    encode . fromMaybe M.empty . M.lookup Own <$> readIORef allBackends
ngxExportIOYY 'getOwnBackends

getBlacklist = const $
    encode <$> readIORef blacklist
ngxExportIOYY 'getBlacklist

getSeqn = C8L.pack . show . seqn . readMsg
ngxExportYY 'getSeqn

getKey = C8L.pack . show . key . readMsg
ngxExportYY 'getKey

getStart = C8L.pack . show . start . readMsg
ngxExportYY 'getStart

getIdx = C8L.pack . show . idx . readMsg
ngxExportYY 'getIdx

getBackend = C8L.pack . backend . readMsg
ngxExportYY 'getBackend

getStatus = C8L.pack . show . status . readMsg
ngxExportYY 'getStatus

