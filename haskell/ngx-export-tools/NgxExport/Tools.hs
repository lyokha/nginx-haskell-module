{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface, TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls, DeriveGeneric, DeriveLift, NumDecimals #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools
-- Copyright   :  (c) Alexey Radkov 2018
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires Template Haskell)
--
-- Extra tools for using in custom Haskell code with
-- <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------


module NgxExport.Tools (
    -- * Various useful functions and data
                        exitWorkerProcess
                       ,terminateWorkerProcess
                       ,ngxRequestPtr
                       ,ngxNow
    -- *** Time intervals
                       ,TimeInterval (..)
                       ,toSec
                       ,threadDelaySec
    -- *** Reading custom types from /ByteStrings/
                       ,readFromByteString
                       ,readFromByteStringAsJSON
    -- * Exporters of simple services
    -- $simpleServices
                       ,ServiceMode (..)
                       ,ngxExportSimpleService
                       ,ngxExportSimpleServiceTyped
                       ,ngxExportSimpleServiceTypedAsJSON
    -- * Re-exported data constructors from /Foreign.C/
    -- | Re-exports are needed by exporters for marshalling in foreign calls.
                       ,Foreign.C.Types.CInt (..)
                       ,Foreign.C.Types.CUInt (..)
                       ) where

import           NgxExport

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.C.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import           Data.Binary.Get
import           Data.IORef
import           Data.Maybe
import           Data.Aeson
import           Control.Monad
import           Control.Concurrent
import           GHC.Generics
import           System.IO.Unsafe (unsafePerformIO)
import           Safe

foreign import ccall "exit" exit :: CInt -> IO ()

-- | Terminates current Nginx worker process.
--
-- Nginx master process shall spawn a new worker process thereafter.
exitWorkerProcess :: IO ()
exitWorkerProcess = exit 1

-- | Terminates current Nginx worker process.
--
-- Nginx master process shall /not/ spawn a new worker process thereafter.
terminateWorkerProcess :: IO ()
terminateWorkerProcess = exit 2

-- | Unmarshals value of Nginx variable __/$_r_ptr/__ into a pointer to the
--   Nginx request object.
--
-- This is safe to use in request-based Haskell handlers such as synchronous
-- and asynchronous tasks and content handlers, but not in services and their
-- derivatives. The value can be passed into a /C plugin/, however, as opposed
-- to usual functions in Nginx C code, it must be tested against the /NULL/
-- value.
ngxRequestPtr :: ByteString -> Ptr ()
ngxRequestPtr = wordPtrToPtr . fromIntegral . runGet getWordhost . L.fromStrict

-- | Returns current time as the number of seconds elapsed since UNIX epoch.
--
-- The value is taken from Nginx core, so no additional system calls get
-- involved. On the other hand, it means that this is only safe to use from
-- an Nginx worker's main thread, i.e. in /synchronous/ Haskell handlers and
-- /service hooks/. Be also aware that this is a small type casting hack:
-- the value is interpreted as being of type @time_t@ while having been
-- actually wrapped in a bigger C struct as its first element.
ngxNow :: IO CTime
ngxNow = ngxCachedTimePtr >>= peek >>= peek . castPtr

-- | Time intervals.
data TimeInterval = Hr Int          -- ^ Hours
                  | Min Int         -- ^ Minutes
                  | Sec Int         -- ^ Seconds
                  | HrMin Int Int   -- ^ Hours and minutes
                  | MinSec Int Int  -- ^ Minutes and seconds
                  deriving (Generic, Lift, Read, Show)
instance FromJSON TimeInterval

-- | Converts a time interval into seconds.
toSec :: TimeInterval -> Int
toSec (Hr h)       = 3600 * h
toSec (Min m)      = 60 * m
toSec (Sec s)      = s
toSec (HrMin h m)  = 3600 * h + 60 * m
toSec (MinSec m s) = 60 * m + s

-- | Delays current thread for the specified number of seconds.
threadDelaySec :: Int -> IO ()
threadDelaySec = threadDelay . (* 1e6)

data Readable a
data ReadableAsJSON a

class FromByteString a where
    type WrappedT a
    fromByteString :: Maybe a -> ByteString -> Maybe (WrappedT a)

instance Read a => FromByteString (Readable a) where
    type WrappedT (Readable a) = a
    fromByteString = const $ readMay . C8.unpack

instance FromJSON a => FromByteString (ReadableAsJSON a) where
    type WrappedT (ReadableAsJSON a) = a
    fromByteString = const decodeStrict

instance FromByteString ByteString where
    type WrappedT ByteString = ByteString
    fromByteString = const Just

-- | Reads a custom type deriving 'Read' from a 'ByteString'.
--
-- Returns 'Nothing' if reading fails.
readFromByteString :: Read a => ByteString -> Maybe a
readFromByteString = fromByteString (Nothing :: Maybe (Readable a))

-- | Reads a custom type deriving 'FromJSON' from a 'ByteString'.
--
-- Returns 'Nothing' if reading fails.
readFromByteStringAsJSON :: FromJSON a => ByteString -> Maybe a
readFromByteStringAsJSON = fromByteString (Nothing :: Maybe (ReadableAsJSON a))

-- $simpleServices
--
-- There are a number of exporters for /simple services/. Here /simplicity/
-- means avoiding boilerplate code regarding to efficient reading of typed
-- configurations and timed restarts of services. All simple services have type
--
-- @
-- 'ByteString' -> 'Prelude.Bool' -> 'IO' 'L.ByteString'
-- @
--
-- which corresponds to the type of usual services from module "NgxExport".
--
-- Below is a toy example.
--
-- File __/test_tools.hs/__.
--
-- @
-- {\-\# LANGUAGE TemplateHaskell, DeriveGeneric, RecordWildCards \#\-}
--
-- module TestTools where
--
-- import           NgxExport.Tools
--
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString.Lazy.Char8 as C8L
-- import           Data.Aeson
-- import           Control.Monad
-- import           GHC.Generics
--
-- test :: ByteString -> Bool -> IO L.ByteString
-- __/test/__ = const . return . L.fromStrict
-- 'ngxExportSimpleService' \'test $
--     'PersistentService' $ Just $ 'Sec' 10
--
-- testRead :: (Read a, Show a) => a -> IO L.ByteString
-- testRead = return . C8L.pack . show
--
-- testReadInt :: Int -> Bool -> IO L.ByteString
-- __/testReadInt/__ = const testRead
-- 'ngxExportSimpleServiceTyped' \'testReadInt \'\'Int $
--     'PersistentService' $ Just $ 'Sec' 10
--
-- newtype Conf = Conf Int deriving (Read, Show)
--
-- testReadConf :: Conf -> Bool -> IO L.ByteString
-- __/testReadConf/__ = const testRead
-- 'ngxExportSimpleServiceTyped' \'testReadConf \'\'Conf $
--     'PersistentService' $ Just $ 'Sec' 10
--
-- data ConfWithDelay = ConfWithDelay { delay :: 'TimeInterval'
--                                    , value :: Int
--                                    } deriving (Read, Show)
--
-- testReadConfWithDelay :: ConfWithDelay -> Bool -> IO L.ByteString
-- __/testReadConfWithDelay/__ c@ConfWithDelay {..} fstRun = do
--     unless fstRun $ 'threadDelaySec' $ 'toSec' delay
--     testRead c
-- 'ngxExportSimpleServiceTyped' \'testReadConfWithDelay \'\'ConfWithDelay $
--     'PersistentService' Nothing
--
-- testReadJSON :: (FromJSON a, Show a) => a -> IO L.ByteString
-- testReadJSON = return . C8L.pack . show
--
-- data ConfJSON = ConfJSONCon1 Int
--               | ConfJSONCon2 deriving (Generic, Show)
-- instance FromJSON ConfJSON
--
-- testReadConfJSON :: ConfJSON -> Bool -> IO L.ByteString
-- __/testReadConfJSON/__ = const testReadJSON
-- 'ngxExportSimpleServiceTypedAsJSON' \'testReadConfJSON \'\'ConfJSON
--     'SingleShotService'
-- @
--
-- Here five simple services of various types are defined: /test/,
-- /testReadInt/, /testReadConf/, /testReadConfWithDelay/, and
-- /testReadConfJSON/. Service /testReadInt/ is not a good example though.
-- The problem is that /typed/ simple services build 'IORef' /storages/ to save
-- their configurations for faster access in future iterations. The name of a
-- storage consists of the name of its type prefixed with __/storage_/__, which
-- means that it's wiser to use custom types or wrappers of well-known types
-- (such as /Conf/ and /ConfWithDelay/) in order to avoid exhaustion of
-- top-level names. In general, this also means that it's not possible to
-- declare in a single Nginx configuration script two or more typed simple
-- services with identical names of their configuration types.
--
-- As soon as all the services in the example merely echo their configurations
-- into their service variables, they must sleep for a while between iterations.
-- Sleeps are managed by strategies defined in type 'ServiceMode'. There are
-- basically three sleeping strategies:
--
-- * Periodical sleeps (for example, @'PersistentService' $ Just $ 'Sec' 10@)
-- * No sleeps between iterations (@'PersistentService' Nothing@)
-- * /Single-shot/ services (@'SingleShotService'@)
--
-- In this toy example the most efficient sleeping strategy is a single-shot
-- service because data is not altered during runtime. Under the hood, the
-- single-shot strategy is implemented as periodical sleeps (with period of
-- @'Hr' 1@), except it runs the handler only on the first iteration, while
-- afterwards it merely returns empty values: as such, this strategy should be
-- accompanied by Nginx directive __/haskell_service_var_ignore_empty/__.
--
-- Notice that service /testReadConfWithDelay/ manages time delays on its own,
-- therefore it uses /no-sleeps/ strategy @'PersistentService' Nothing@.
--
-- File __/nginx.conf/__.
--
-- @
-- user                    nobody;
-- worker_processes        2;
--
-- events {
--     worker_connections  1024;
-- }
--
-- http {
--     default_type        application\/octet-stream;
--     sendfile            on;
--
--     haskell load \/var\/lib\/nginx\/test_tools.so;
--
--     haskell_run_service __/simpleService_test/__
--             $hs_test
--             test;
--
--     haskell_run_service __/simpleService_testReadInt/__
--             $hs_testReadInt
--             5000000;
--
--     haskell_run_service __/simpleService_testReadConf/__
--             $hs_testReadConf
--             \'Conf 20\';
--
--     haskell_run_service __/simpleService_testReadConfWithDelay/__
--             $hs_testReadConfWithDelay
--             \'ConfWithDelay { delay = Sec 10, value = 12 }\';
--
--     haskell_run_service __/simpleService_testReadConfJSON/__
--             $hs_testReadConfJSON
--             \'{\"tag\":\"ConfJSONCon1\", \"contents\":56}\';
--
--     haskell_service_var_ignore_empty $hs_testReadConfJSON;
--
--     server {
--         listen       8010;
--         server_name  main;
--         error_log    \/tmp\/nginx-test-haskell-error.log;
--         access_log   \/tmp\/nginx-test-haskell-access.log;
--
--         location \/ {
--             echo \"Service variables:\";
--             echo \"  hs_test: $hs_test\";
--             echo \"  hs_testReadInt: $hs_testReadInt\";
--             echo \"  hs_testReadConf: $hs_testReadConf\";
--             echo \"  hs_testReadConfWithDelay: $hs_testReadConfWithDelay\";
--             echo \"  hs_testReadConfJSON: $hs_testReadConfJSON\";
--         }
--     }
-- }
-- @
--
-- Notice that Haskel handlers defined in /test_tools.hs/ are referred from
-- the Nginx configuration file with prefix __/simpleService_/__.
--
-- Let's run a simple test.
--
-- > $ curl 'http://localhost:8010/'
-- > Service variables:
-- >   hs_test: test
-- >   hs_testReadInt: 5000000
-- >   hs_testReadConf: Conf 20
-- >   hs_testReadConfWithDelay: ConfWithDelay {delay = Sec 10, value = 12}
-- >   hs_testReadConfJSON: ConfJSONCon1 56

-- | Defines a sleeping strategy.
--
-- Single-shot services should be accompanied by Nginx directive
-- __/haskell_service_var_ignore_empty/__.
data ServiceMode
    -- | Persistent service (with or without periodical sleeps)
    = PersistentService (Maybe TimeInterval)
    -- | Single-shot service
    | SingleShotService

simpleServiceWrap ::
    (a -> Bool -> IO L.ByteString) -> a -> Bool -> IO L.ByteString
simpleServiceWrap f = f

ngxExportSimpleService' :: Name -> Maybe (Name, Bool) -> ServiceMode -> Q [Dec]
ngxExportSimpleService' f c m = do
    confBs <- newName "confBs_"
    fstRun <- newName "fstRun_"
    let nameSsf = mkName $ "simpleService_" ++ nameBase f
        hasConf = isJust c
        (sNameC, typeC, isJSON) =
            if hasConf
                then let c' = fromJust c
                         tName = nameBase $ fst c'
                     in (mkName $ "storage_" ++ tName
                        ,conT $ mkName tName
                        ,snd c'
                        )
                else (mkName "storage_dummy__"
                     ,conT $ mkName "Dummy__"
                     ,False
                     )
        initConf =
            let eConfBs = varE confBs
            in if hasConf
                   then let storage = varE sNameC
                            readConf = if isJSON
                                           then [|readFromByteStringAsJSON|]
                                           else [|readFromByteString|]
                        in [|readIORef $(storage) >>=
                                 maybe (do
                                            let conf_data__ =
                                                    $(readConf) $(eConfBs)
                                            when (isNothing conf_data__)
                                                terminateWorkerProcess
                                            writeIORef $(storage) conf_data__
                                            return conf_data__
                                       ) (return . Just)
                           |]
                   else [|return $
                              fromByteString (Nothing :: Maybe ByteString)
                                  $(eConfBs)
                        |]
        (waitTime, serviceWrap) =
            let eF = varE f
                eFstRun = varE fstRun
            in case m of
                   PersistentService i ->
                       (if isJust i
                            then let t = fromJust i
                                 in [|unless $(eFstRun) $
                                          threadDelaySec $ toSec t
                                    |]
                            else [|return ()|]
                       ,[|\conf_data__ ->
                              simpleServiceWrap
                                  $(eF) (fromJust conf_data__) $(eFstRun)
                        |]
                       )
                   SingleShotService ->
                       ([|unless $(eFstRun) $
                              threadDelaySec $ toSec $ Hr 1|]
                       ,[|\conf_data__ ->
                              if $(eFstRun)
                                  then simpleServiceWrap
                                           $(eF) (fromJust conf_data__)
                                               $(eFstRun)
                                  else return L.empty
                        |]
                       )
    concat <$> sequence
        [sequence $
            (if hasConf
                 then [sigD sNameC [t|IORef (Maybe $(typeC))|]
                      ,funD sNameC [clause []
                                       (normalB
                                           [|unsafePerformIO $
                                                 newIORef Nothing
                                           |]
                                       )
                                       []
                                  ]
                      ,pragInlD sNameC NoInline FunLike AllPhases
                      ]
                 else []
            )
            ++
            [sigD nameSsf [t|ByteString -> Bool -> IO L.ByteString|]
            ,funD nameSsf
                [clause [varP confBs, varP fstRun]
                    (normalB [|$(waitTime) >> $(initConf) >>= $(serviceWrap)|])
                    []
                ]
            ]
        ,ngxExportServiceIOYY nameSsf
        ]

-- | Exports a simple service with specified name and service mode.
--
-- The service expects a plain 'ByteString' object as its first argument.
ngxExportSimpleService :: Name         -- ^ Name of the service
                       -> ServiceMode  -- ^ Service mode
                       -> Q [Dec]
ngxExportSimpleService f =
    ngxExportSimpleService' f Nothing

-- | Exports a simple service with specified name and service mode.
--
-- The service expects an object of a custom type deriving 'Read' as its
-- first argument. For the sake of efficiency, this object gets deserialized
-- into a global 'IORef' data storage on the first service run to be further
-- accessed directly from this storage. The storage can be accessed from
-- elsewhere by name comprised of the name of the custom type prefixed with
-- __/storage_/__. The stored data is wrapped in 'Maybe' container.
ngxExportSimpleServiceTyped :: Name         -- ^ Name of the service
                            -> Name         -- ^ Name of the custom type
                            -> ServiceMode  -- ^ Service mode
                            -> Q [Dec]
ngxExportSimpleServiceTyped f c =
    ngxExportSimpleService' f $ Just (c, False)

-- | Exports a simple service with specified name and service mode.
--
-- The service expects an object of a custom type deriving 'FromJSON' as its
-- first argument. For the sake of efficiency, this object gets deserialized
-- into a global 'IORef' data storage on the first service run to be further
-- accessed directly from this storage. The storage can be accessed from
-- elsewhere by name comprised of the name of the custom type prefixed with
-- __/storage_/__. The stored data is wrapped in 'Maybe' container.
ngxExportSimpleServiceTypedAsJSON :: Name         -- ^ Name of the service
                                  -> Name         -- ^ Name of the custom type
                                  -> ServiceMode  -- ^ Service mode
                                  -> Q [Dec]
ngxExportSimpleServiceTypedAsJSON f c =
    ngxExportSimpleService' f $ Just (c, True)

