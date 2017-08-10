{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport
-- Copyright   :  (c) Alexey Radkov 2016-2017
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires POSIX)
--
-- Export regular haskell functions for using in directives of
-- <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------

module NgxExport (
    -- * Exporters
                  ngxExportSS
                 ,ngxExportSSS
                 ,ngxExportSLS
                 ,ngxExportBS
                 ,ngxExportBSS
                 ,ngxExportBLS
                 ,ngxExportYY
                 ,ngxExportBY
                 ,ngxExportIOYY
                 ,ngxExportAsyncIOYY
                 ,ngxExportAsyncOnReqBody
                 ,ngxExportServiceIOYY
                 ,ngxExportHandler
                 ,ngxExportDefHandler
                 ,ngxExportUnsafeHandler
    -- * Re-exported data constructors from /"Foreign.C"/
    --   (for marshalling in foreign calls)
                 ,Foreign.C.CInt (..)
                 ,Foreign.C.CUInt (..)) where

import           Language.Haskell.TH
import           Foreign.C
import           Foreign.Ptr
import           Foreign.StablePtr
import           Foreign.Storable
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           System.IO.Error
import           System.Posix.IO
import           Control.Monad
import           Control.Exception hiding (Handler)
import           GHC.IO.Exception (ioe_errno)
import           Control.Concurrent.Async
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.Binary.Put
import           Paths_ngx_export (version)
import           Data.Version

pattern I i        <- (fromIntegral -> i)
pattern PtrLen s l <- (s, I l)
pattern ToBool i   <- (toBool -> i)
pattern EmptyLBS   <- (L.null -> True)

data NgxExport = SS            (String -> String)
               | SSS           (String -> String -> String)
               | SLS           ([String] -> String)
               | BS            (String -> Bool)
               | BSS           (String -> String -> Bool)
               | BLS           ([String] -> Bool)
               | YY            (B.ByteString -> L.ByteString)
               | BY            (B.ByteString -> Bool)
               | IOYY          (B.ByteString -> Bool -> IO L.ByteString)
               | IOYYY         (L.ByteString -> B.ByteString -> IO L.ByteString)
               | Handler       (B.ByteString -> (L.ByteString, String, Int))
               | UnsafeHandler (B.ByteString ->
                                    (B.ByteString, B.ByteString, Int))

let name = mkName "exportType" in do
    TyConI (DataD _ _ _ _ cs _) <- reify ''NgxExport
    let cons = map (\(NormalC con _) -> con) cs
    sequence
        [sigD name [t|NgxExport -> IO CInt|],
         funD name $
             map (\(c, i) -> clause [conP c [wildP]] (normalB [|return i|]) [])
                 (zip cons [1 ..] :: [(Name, Int)])
        ]

ngxExport' :: (Name -> Q Exp) -> Name -> Name -> Q Type -> Name -> Q [Dec]
ngxExport' m e h t f = sequence
    [sigD nameFt typeFt,
     funD nameFt $ body [|exportType $efVar|],
     ForeignD . ExportF CCall ftName nameFt <$> typeFt,
     sigD nameF t,
     funD nameF $ body [|$hVar $efVar|],
     ForeignD . ExportF CCall fName nameF <$> t
    ]
    where hVar   = varE h
          efVar  = conE e `appE` m f
          fName  = "ngx_hs_" ++ nameBase f
          nameF  = mkName fName
          ftName = "type_" ++ fName
          nameFt = mkName ftName
          typeFt = [t|IO CInt|]
          body b = [clause [] (normalB b) []]

ngxExport :: Name -> Name -> Q Type -> Name -> Q [Dec]
ngxExport = ngxExport' varE

ngxExportC :: Name -> Name -> Q Type -> Name -> Q [Dec]
ngxExportC = ngxExport' $ infixE (Just $ varE 'const) (varE '(.)) . Just . varE

-- | Exports a function of type
-- /'String' -> 'String'/
-- for using in directive /haskell_run/.
ngxExportSS =
    ngxExport 'SS 'sS
    [t|CString -> CInt ->
       Ptr CString -> Ptr CInt -> IO CUInt|]

-- | Exports a function of type
-- /'String' -> 'String' -> 'String'/
-- for using in directive /haskell_run/.
ngxExportSSS =
    ngxExport 'SSS 'sSS
    [t|CString -> CInt -> CString -> CInt ->
       Ptr CString -> Ptr CInt -> IO CUInt|]

-- | Exports a function of type
-- /['String'] -> 'String'/
-- for using in directive /haskell_run/.
ngxExportSLS =
    ngxExport 'SLS 'sLS
    [t|Ptr NgxStrType -> CInt ->
       Ptr CString -> Ptr CInt -> IO CUInt|]

-- | Exports a function of type
-- /'String' -> 'Bool'/
-- for using in directive /haskell_run/.
ngxExportBS =
    ngxExport 'BS 'bS
    [t|CString -> CInt ->
       Ptr CString -> Ptr CInt -> IO CUInt|]

-- | Exports a function of type
-- /'String' -> 'String' -> 'Bool'/
-- for using in directive /haskell_run/.
ngxExportBSS =
    ngxExport 'BSS 'bSS
    [t|CString -> CInt -> CString -> CInt ->
       Ptr CString -> Ptr CInt -> IO CUInt|]

-- | Exports a function of type
-- /['String'] -> 'Bool'/
-- for using in directive /haskell_run/.
ngxExportBLS =
    ngxExport 'BLS 'bLS
    [t|Ptr NgxStrType -> CInt ->
       Ptr CString -> Ptr CInt -> IO CUInt|]

-- | Exports a function of type
-- /'B.ByteString' -> 'L.ByteString'/
-- for using in directive /haskell_run/.
ngxExportYY =
    ngxExport 'YY 'yY
    [t|CString -> CInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt ->
       Ptr (StablePtr L.ByteString) -> IO CUInt|]

-- | Exports a function of type
-- /'B.ByteString' -> 'Bool'/
-- for using in directive /haskell_run/.
ngxExportBY =
    ngxExport 'BY 'bY
    [t|CString -> CInt ->
       Ptr CString -> Ptr CInt -> IO CUInt|]

-- | Exports a function of type
-- /'B.ByteString' -> 'IO' 'L.ByteString'/
-- for using in directive /haskell_run/.
ngxExportIOYY =
    ngxExportC 'IOYY 'ioyY
    [t|CString -> CInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt ->
       Ptr (StablePtr L.ByteString) -> IO CUInt|]

-- | Exports a function of type
-- /'B.ByteString' -> 'IO' 'L.ByteString'/
-- for using in directive /haskell_run_async/.
ngxExportAsyncIOYY =
    ngxExportC 'IOYY 'asyncIOYY
    [t|CString -> CInt -> CInt -> CUInt -> CUInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr CUInt ->
       Ptr (StablePtr L.ByteString) -> IO ()|]

-- | Exports a function of type
-- /'L.ByteString' -> 'B.ByteString' -> 'IO' 'L.ByteString'/
-- for using in directive /haskell_run_async_on_request_body/.
--
-- The first argument of the exported function contains buffers of the client
-- request body.
ngxExportAsyncOnReqBody =
    ngxExport 'IOYYY 'asyncIOYYY
    [t|Ptr NgxStrType -> CInt -> CString -> CInt -> CInt -> CUInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr CUInt ->
       Ptr (StablePtr L.ByteString) -> IO ()|]

-- | Exports a function of type
-- /'B.ByteString' -> 'Bool' -> 'IO' 'L.ByteString'/
-- for using in directive /haskell_run_service/.
--
-- The boolean argument of the exported function marks that the service is
-- being run for the first time.
ngxExportServiceIOYY =
    ngxExport 'IOYY 'asyncIOYY
    [t|CString -> CInt -> CInt -> CUInt -> CUInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr CUInt ->
       Ptr (StablePtr L.ByteString) -> IO ()|]

-- | Exports a function of type
-- /'B.ByteString' -> ('L.ByteString', 'String', 'Int')/
-- for using in directives /haskell_content/ and /haskell_static_content/.
--
-- The first element in the returned /3-tuple/ of the exported function is
-- the /content/, the second is the /content type/, and the third is the
-- /HTTP status/.
ngxExportHandler =
    ngxExport 'Handler 'handler
    [t|CString -> CInt -> Ptr (Ptr NgxStrType) -> Ptr CInt ->
       Ptr CString -> Ptr CSize -> Ptr CInt ->
       Ptr (StablePtr L.ByteString) -> IO CUInt|]

-- | Exports a function of type
-- /'B.ByteString' -> 'L.ByteString'/
-- for using in directives /haskell_content/ and /haskell_static_content/.
ngxExportDefHandler =
    ngxExport 'YY 'defHandler
    [t|CString -> CInt ->
       Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr CString ->
       Ptr (StablePtr L.ByteString) -> IO CUInt|]

-- | Exports a function of type
-- /'B.ByteString' -> ('B.ByteString', 'B.ByteString', 'Int')/
-- for using in directive /haskell_unsafe_content/.
--
-- The first element in the returned /3-tuple/ of the exported function is
-- the /content/, the second is the /content type/, and the third is the
-- /HTTP status/. Both the content and the content type are supposed to be
-- referring to low-level string literals which do not need to be freed upon
-- the request termination and must not be garbage-collected in the Haskell RTS.
ngxExportUnsafeHandler =
    ngxExport 'UnsafeHandler 'unsafeHandler
    [t|CString -> CInt -> Ptr CString -> Ptr CSize ->
       Ptr CString -> Ptr CSize -> Ptr CInt -> IO CUInt|]

data NgxStrType = NgxStrType CSize CString

instance Storable NgxStrType where
    alignment _ = max (alignment (undefined :: CSize))
                      (alignment (undefined :: CString))
    sizeOf = (2 *) . alignment  -- must always be correct for
                                -- aligned struct ngx_str_t
    peek p = do
        n <- peekByteOff p 0
        s <- peekByteOff p $ alignment (undefined :: NgxStrType)
        return $ NgxStrType n s
    poke p x@(NgxStrType n s) = do
        poke (castPtr p) n
        poke (plusPtr p $ alignment x) s

safeMallocBytes :: Int -> IO (Ptr a)
safeMallocBytes =
    flip catchIOError (const $ return nullPtr) . mallocBytes

peekNgxStringArrayLen :: Ptr NgxStrType -> Int -> IO [String]
peekNgxStringArrayLen x n = sequence $
    foldr (\k ->
              ((peekElemOff x k >>=
                  (\(NgxStrType (I m) y) ->
                      peekCStringLen (y, m))) :)) [] [0 .. n - 1]

peekNgxStringArrayLenY :: Ptr NgxStrType -> Int -> IO L.ByteString
peekNgxStringArrayLenY x n = L.fromChunks <$> sequence
    (foldr (\k ->
              ((peekElemOff x k >>=
                  (\(NgxStrType (I m) y) ->
                      B.unsafePackCStringLen (y, m))) :)) [] [0 .. n - 1])

pokeCStringLen :: (Storable a, Num a) =>
    CString -> a -> Ptr CString -> Ptr a -> IO ()
pokeCStringLen x n p s = poke p x >> poke s n

toBuffers :: L.ByteString -> IO (Ptr NgxStrType, Int)
toBuffers EmptyLBS =
    return (nullPtr, 0)
toBuffers s = do
    t <- safeMallocBytes $
        L.foldlChunks (const . succ) 0 s * sizeOf (undefined :: NgxStrType)
    if t == nullPtr
        then return (nullPtr, -1)
        else (,) t <$>
                L.foldlChunks
                    (\a s -> do
                        off <- a
                        B.unsafeUseAsCStringLen s $
                            \(s, l) -> pokeElemOff t off $
                                NgxStrType (fromIntegral l) s
                        return $ off + 1
                    ) (return 0) s

pokeLazyByteString :: L.ByteString ->
    Ptr (Ptr NgxStrType) -> Ptr CInt ->
    Ptr (StablePtr L.ByteString) -> IO ()
pokeLazyByteString s p pl spd = do
    PtrLen t l <- toBuffers s
    poke p t >> poke pl l
    when (t /= nullPtr) $ newStablePtr s >>= poke spd

safeNewCStringLen :: String -> IO CStringLen
safeNewCStringLen =
    flip catchIOError (const $ return (nullPtr, -1)) . newCStringLen

safeHandler :: Ptr CString -> Ptr CInt -> IO CUInt -> IO CUInt
safeHandler p pl = handle $ \e -> do
    PtrLen x l <- safeNewCStringLen $ show (e :: SomeException)
    pokeCStringLen x l p pl
    return 1

sS :: NgxExport -> CString -> CInt ->
    Ptr CString -> Ptr CInt -> IO CUInt
sS (SS f) x (I n) p pl =
    safeHandler p pl $ do
        PtrLen s l <- f <$> peekCStringLen (x, n)
                        >>= newCStringLen
        pokeCStringLen s l p pl
        return 0

sSS :: NgxExport -> CString -> CInt -> CString -> CInt ->
    Ptr CString -> Ptr CInt -> IO CUInt
sSS (SSS f) x (I n) y (I m) p pl =
    safeHandler p pl $ do
        PtrLen s l <- f <$> peekCStringLen (x, n)
                        <*> peekCStringLen (y, m)
                        >>= newCStringLen
        pokeCStringLen s l p pl
        return 0

sLS :: NgxExport -> Ptr NgxStrType -> CInt ->
    Ptr CString -> Ptr CInt -> IO CUInt
sLS (SLS f) x (I n) p pl =
    safeHandler p pl $ do
        PtrLen s l <- f <$> peekNgxStringArrayLen x n
                        >>= newCStringLen
        pokeCStringLen s l p pl
        return 0

yY :: NgxExport -> CString -> CInt ->
    Ptr (Ptr NgxStrType) -> Ptr CInt ->
    Ptr (StablePtr L.ByteString) -> IO CUInt
yY (YY f) x (I n) p pl spd = do
    (s, r) <- (flip (,) 0 . f <$> B.unsafePackCStringLen (x, n))
                `catch` \e -> return (C8L.pack $ show (e :: SomeException), 1)
    pokeLazyByteString s p pl spd
    return r

ioyY :: NgxExport -> CString -> CInt ->
    Ptr (Ptr NgxStrType) -> Ptr CInt ->
    Ptr (StablePtr L.ByteString) -> IO CUInt
ioyY (IOYY f) x (I n) p pl spd = do
    (s, r) <- (do
                  s <- B.unsafePackCStringLen (x, n) >>= flip f False
                  fmap (flip (,) 0) $ return $! s
              ) `catch` \e -> return (C8L.pack $ show (e :: SomeException), 1)
    pokeLazyByteString s p pl spd
    return r

asyncIOFlag1b :: B.ByteString
asyncIOFlag1b = L.toStrict $ runPut $ putInt8 1

asyncIOFlag8b :: B.ByteString
asyncIOFlag8b = L.toStrict $ runPut $ putInt64host 1

asyncIOCommon :: IO C8L.ByteString ->
    CInt -> Bool -> Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr CUInt ->
    Ptr (StablePtr L.ByteString) -> IO ()
asyncIOCommon a (I fd) efd p pl r spd = void . async $ do
    (do {s <- a; fmap Right $ return $! s})
        `catch` (\e -> return $ Left $ show (e :: SomeException)) >>=
            either (pokeAll 1 . C8L.pack) (pokeAll 0)
    uninterruptibleMask_ $
        if efd
            then writeFlag8b
            else writeFlag1b >> closeFd fd `catchIOError` const (return ())
    where pokeAll e s = pokeLazyByteString s p pl spd >> poke r e
          writeBufN n s w
              | w < n = (w +) <$>
                  fdWriteBuf fd (plusPtr s $ fromIntegral w) (n - w)
                  `catchIOError`
                  (\e -> return $
                       if ioe_errno e == Just ((\(Errno e) -> e) eINTR)
                           then 0
                           else n
                  ) >>= writeBufN n s
              | otherwise = return w
          writeFlag1b = void $
              B.unsafeUseAsCString asyncIOFlag1b $ flip (writeBufN 1) 0
          writeFlag8b = void $
              B.unsafeUseAsCString asyncIOFlag8b $ flip (writeBufN 8) 0

asyncIOYY :: NgxExport -> CString -> CInt ->
    CInt -> CUInt -> CUInt -> Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr CUInt ->
    Ptr (StablePtr L.ByteString) -> IO ()
asyncIOYY (IOYY f) x (I n) fd (ToBool efd) (ToBool fstRun) =
    asyncIOCommon (B.unsafePackCStringLen (x, n) >>= flip f fstRun) fd efd

asyncIOYYY :: NgxExport -> Ptr NgxStrType -> CInt -> CString -> CInt ->
    CInt -> CUInt -> Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr CUInt ->
    Ptr (StablePtr L.ByteString) -> IO ()
asyncIOYYY (IOYYY f) b (I m) x (I n) fd (ToBool efd) =
    asyncIOCommon
    (do
        b' <- peekNgxStringArrayLenY b m
        x' <- B.unsafePackCStringLen (x, n)
        f b' x'
    ) fd efd

bS :: NgxExport -> CString -> CInt ->
    Ptr CString -> Ptr CInt -> IO CUInt
bS (BS f) x (I n) p pl =
    safeHandler p pl $ do
        r <- fromBool . f <$> peekCStringLen (x, n)
        pokeCStringLen nullPtr 0 p pl
        return r

bSS :: NgxExport -> CString -> CInt -> CString -> CInt ->
    Ptr CString -> Ptr CInt -> IO CUInt
bSS (BSS f) x (I n) y (I m) p pl =
    safeHandler p pl $ do
        r <- (fromBool .) . f <$> peekCStringLen (x, n)
                              <*> peekCStringLen (y, m)
        pokeCStringLen nullPtr 0 p pl
        return r

bLS :: NgxExport -> Ptr NgxStrType -> CInt ->
    Ptr CString -> Ptr CInt -> IO CUInt
bLS (BLS f) x (I n) p pl =
    safeHandler p pl $ do
        r <- fromBool . f <$> peekNgxStringArrayLen x n
        pokeCStringLen nullPtr 0 p pl
        return r

bY :: NgxExport -> CString -> CInt ->
    Ptr CString -> Ptr CInt -> IO CUInt
bY (BY f) x (I n) p pl =
    safeHandler p pl $ do
        r <- fromBool . f <$> B.unsafePackCStringLen (x, n)
        pokeCStringLen nullPtr 0 p pl
        return r

handler :: NgxExport -> CString -> CInt -> Ptr (Ptr NgxStrType) -> Ptr CInt ->
    Ptr CString -> Ptr CSize -> Ptr CInt ->
    Ptr (StablePtr L.ByteString) -> IO CUInt
handler (Handler f) x (I n) p pl pct plct pst spd =
    safeHandler pct pst $ do
        (s, ct, I st) <- f <$> B.unsafePackCStringLen (x, n)
        PtrLen t l <- toBuffers s
        poke p t >> poke pl l
        PtrLen sct lct <- newCStringLen ct
        pokeCStringLen sct lct pct plct >> poke pst st
        when (t /= nullPtr) $ newStablePtr s >>= poke spd
        return 0

defHandler :: NgxExport -> CString -> CInt ->
    Ptr (Ptr NgxStrType) -> Ptr CInt -> Ptr CString ->
    Ptr (StablePtr L.ByteString) -> IO CUInt
defHandler (YY f) x (I n) p pl pe spd =
    safeHandler pe pl $ do
        s <- f <$> B.unsafePackCStringLen (x, n)
        pokeLazyByteString s p pl spd
        return 0

unsafeHandler :: NgxExport -> CString -> CInt -> Ptr CString -> Ptr CSize ->
    Ptr CString -> Ptr CSize -> Ptr CInt -> IO CUInt
unsafeHandler (UnsafeHandler f) x (I n) p pl pct plct pst =
    safeHandler pct pst $ do
        (s, ct, I st) <- f <$> B.unsafePackCStringLen (x, n)
        PtrLen t l <- B.unsafeUseAsCStringLen s return
        pokeCStringLen t l p pl
        PtrLen sct lct <- B.unsafeUseAsCStringLen ct return
        pokeCStringLen sct lct pct plct >> poke pst st
        return 0

foreign export ccall ngxExportReleaseLockedByteString ::
    StablePtr L.ByteString -> IO ()

ngxExportReleaseLockedByteString :: StablePtr L.ByteString -> IO ()
ngxExportReleaseLockedByteString = freeStablePtr

foreign export ccall ngxExportVersion :: Ptr CInt -> CInt -> IO CInt

ngxExportVersion :: Ptr CInt -> CInt -> IO CInt
ngxExportVersion x (I n) = fromIntegral <$>
    foldM (\k (I v) -> pokeElemOff x k v >> return (k + 1)) 0
        (take n $ versionBranch version)

