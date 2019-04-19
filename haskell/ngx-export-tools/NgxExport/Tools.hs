{-# LANGUAGE TemplateHaskell, TypeFamilies, EmptyDataDecls #-}
{-# LANGUAGE DeriveGeneric, DeriveLift, NumDecimals #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools
-- Copyright   :  (c) Alexey Radkov 2018-2019
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
                        terminateWorkerProcess
                       ,restartWorkerProcess
                       ,finalizeHTTPRequest
                       ,ngxRequestPtr
                       ,ngxNow
                       ,ngxPid
    -- *** Time intervals
                       ,TimeInterval (..)
                       ,toSec
                       ,threadDelaySec
    -- *** Reading custom types from /ByteStrings/
    -- $readingCustomTypes
                       ,readFromByteString
                       ,readFromByteStringAsJSON
                       ,readFromByteStringWithRPtr
                       ,readFromByteStringWithRPtrAsJSON
    -- * Exporters of simple services
    -- $simpleServices
                       ,ServiceMode (..)
                       ,ngxExportSimpleService
                       ,ngxExportSimpleServiceTyped
                       ,ngxExportSimpleServiceTypedAsJSON
    -- * Split services
    -- $splitServices
                       ,splitService
                       ,ignitionService
                       ,deferredService
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
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import           Data.Binary.Get
import           Data.IORef
import           Data.Maybe
import           Data.Aeson
import           Control.Monad
import           Control.Arrow
import           Control.Exception
import           Control.Concurrent
import           GHC.Generics
import           System.IO.Unsafe (unsafePerformIO)
import           System.Posix.Types
import           Safe

-- | Terminates the Nginx worker process from a Haskell service.
--
-- Nginx master process shall /not/ spawn a new worker process thereafter. This
-- function throws exception 'TerminateWorkerProcess', and therefore terminates
-- the worker process effectively only from a Haskell service.
terminateWorkerProcess :: String -> IO ()
terminateWorkerProcess = throwIO . TerminateWorkerProcess

-- | Restarts the Nginx worker process from a Haskell service.
--
-- Nginx master process shall spawn a new worker process after termination of
-- the current one. This function throws exception 'RestartWorkerProcess', and
-- therefore terminates the worker process effectively only from a Haskell
-- service.
restartWorkerProcess :: String -> IO ()
restartWorkerProcess = throwIO . RestartWorkerProcess

-- | Finalizes the current HTTP request from a Haskell asynchronous variable
--   handler.
--
-- This function throws exception 'FinalizeHTTPRequest', and therefore
-- terminates the HTTP request effectively only from a Haskell asynchronous
-- variable handler.
finalizeHTTPRequest :: Int -> Maybe String -> IO ()
finalizeHTTPRequest = (throwIO .) . FinalizeHTTPRequest

-- | Unmarshals the value of Nginx variable __/$_r_ptr/__ into a pointer to
--   the Nginx request object.
--
-- This is safe to use in request-based Haskell handlers such as synchronous
-- and asynchronous tasks and content handlers, but not in services and their
-- derivatives. In /asynchronous/ tasks and content handlers the value must be
-- used as read-only. The value can be passed into a /C plugin/, however, as
-- opposed to usual functions in Nginx C code, it must be tested against the
-- /NULL/ value.
ngxRequestPtr :: ByteString -> Ptr ()
ngxRequestPtr = wordPtrToPtr . fromIntegral . runGet getWordhost . L.fromStrict

-- | Returns the current time as the number of seconds elapsed since the UNIX
--   epoch.
--
-- The value is taken from Nginx core, so no additional system calls get
-- involved. On the other hand, it means that this is only safe to use from
-- an Nginx worker's main thread, i.e. in /synchronous/ Haskell handlers and
-- /service hooks/. Be also aware that this is a small type casting hack:
-- the value is interpreted as being of type @time_t@ while having been
-- actually wrapped in a bigger C struct as its first element.
ngxNow :: IO CTime
ngxNow = ngxCachedTimePtr >>= peek >>= peek . castPtr

-- | Returns the /PID/ of the current worker process cached in Nginx core.
ngxPid :: IO CPid
ngxPid = ngxCachedPid

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

-- | Delays the current thread for the specified number of seconds.
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

-- $readingCustomTypes
--
-- There are a number of functions to support /typed/ exchange between Nginx
-- and Haskell handlers. Functions 'readFromByteString' and
-- 'readFromByteStringAsJSON' expect serialized values of custom types deriving
-- or implementing instances of 'Read' and 'FromJSON' respectively. Functions
-- 'readFromByteStringWithRPtr' and 'readFromByteStringWithRPtrAsJSON'
-- additionally expect a binary value of a C pointer size marshalled in front
-- of the value of the custom type. This pointer should correspond to the value
-- of Nginx variable __/$_r_ptr/__.
--
-- Below is a toy example.
--
-- ==== File /test_tools.hs/
-- @
-- {-\# LANGUAGE TemplateHaskell, DeriveGeneric, TypeApplications \#-}
--
-- module TestTools where
--
-- import           NgxExport
-- import           NgxExport.Tools
--
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString.Lazy.Char8 as C8L
-- import           Data.Aeson
-- import           GHC.Generics
--
-- showAsLazyByteString :: Show a => a -> L.ByteString
-- showAsLazyByteString = C8L.pack . show
--
-- newtype Conf = Conf Int deriving (Read, Show)
--
-- data ConfJSON = ConfJSONCon1 Int
--               | ConfJSONCon2 deriving (Generic, Show)
-- instance FromJSON ConfJSON
--
-- testReadIntHandler :: ByteString -> L.ByteString
-- __/testReadIntHandler/__ = showAsLazyByteString .
--     'readFromByteString' \@Int
-- 'ngxExportYY' \'testReadIntHandler
--
-- testReadConfHandler :: ByteString -> L.ByteString
-- __/testReadConfHandler/__ = showAsLazyByteString .
--     'readFromByteString' \@Conf
-- 'ngxExportYY' \'testReadConfHandler
--
-- testReadConfJSONHandler :: ByteString -> IO L.ByteString
-- __/testReadConfJSONHandler/__ = return . showAsLazyByteString .
--     'readFromByteStringAsJSON' \@ConfJSON
-- 'ngxExportAsyncIOYY' \'testReadConfJSONHandler
--
-- testReadConfWithRPtrHandler :: ByteString -> L.ByteString
-- __/testReadConfWithRPtrHandler/__ = showAsLazyByteString .
--     'readFromByteStringWithRPtr' \@Conf
-- 'ngxExportYY' \'testReadConfWithRPtrHandler
--
-- testReadConfWithRPtrJSONHandler :: ByteString -> L.ByteString
-- __/testReadConfWithRPtrJSONHandler/__ = showAsLazyByteString .
--     'readFromByteStringWithRPtrAsJSON' \@ConfJSON
-- 'ngxExportYY' \'testReadConfWithRPtrJSONHandler
-- @
--
-- Here five Haskell handlers are defined: /testReadIntHandler/,
-- /testReadConfHandler/, /testReadConfJSONHandler/,
-- /testReadConfWithRPtrHandler/, and /testReadConfWithRPtrJSONHandler/. Four
-- of them are /synchronous/ and one is /asynchronous/ for the sake of variety.
--
-- ==== File /nginx.conf/
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
--     server {
--         listen       8010;
--         server_name  main;
--         error_log    \/tmp\/nginx-test-haskell-error.log;
--         access_log   \/tmp\/nginx-test-haskell-access.log;
--
--         location \/ {
--             haskell_run __/testReadIntHandler/__
--                     $hs_testReadIntHandler
--                     -456;
--             haskell_run __/testReadConfHandler/__
--                     $hs_testReadConfHandler
--                     \'Conf 21\';
--             haskell_run_async __/testReadConfJSONHandler/__
--                     $hs_testReadConfJSONHandler
--                     \'{\"tag\":\"ConfJSONCon2\"}\';
--             haskell_run_async __/testReadConfJSONHandler/__
--                     $hs_testReadConfJSONHandlerBadInput
--                     \'{\"tag\":\"Unknown\"}\';
--             haskell_run __/testReadConfWithRPtrHandler/__
--                     $hs_testReadConfWithRPtrHandler
--                     \'${_r_ptr}Conf 21\';
--             haskell_run __/testReadConfWithRPtrJSONHandler/__
--                     $hs_testReadConfWithRPtrJSONHandler
--                     \'$_r_ptr
--                      {\"tag\":\"ConfJSONCon1\", \"contents\":4}
--                     \';
--
--             echo \"Handler variables:\";
--             echo \"  hs_testReadIntHandler: $hs_testReadIntHandler\";
--             echo \"  hs_testReadConfHandler: $hs_testReadConfHandler\";
--             echo \"  hs_testReadConfJSONHandler: $hs_testReadConfJSONHandler\";
--             echo \"  hs_testReadConfJSONHandlerBadInput: $hs_testReadConfJSONHandlerBadInput\";
--             echo \"  hs_testReadConfWithRPtrHandler: $hs_testReadConfWithRPtrHandler\";
--             echo \"  hs_testReadConfWithRPtrJSONHandler: $hs_testReadConfWithRPtrJSONHandler\";
--         }
--     }
-- }
-- @
--
-- ==== A simple test
-- > $ curl 'http://localhost:8010/'
-- > Handler variables:
-- >   hs_testReadIntHandler: Just (-456)
-- >   hs_testReadConfHandler: Just (Conf 21)
-- >   hs_testReadConfJSONHandler: Just ConfJSONCon2
-- >   hs_testReadConfJSONHandlerBadInput: Nothing
-- >   hs_testReadConfWithRPtrHandler: (0x00000000016fc790,Just (Conf 21))
-- >   hs_testReadConfWithRPtrJSONHandler: (0x00000000016fc790,Just (ConfJSONCon1 4))

-- | Reads an object of a custom type implementing an instance of 'Read'
--   from a 'ByteString'.
--
-- Returns 'Nothing' if reading fails.
readFromByteString :: Read a => ByteString -> Maybe a
readFromByteString = fromByteString (Nothing :: Maybe (Readable a))

-- | Reads an object of a custom type implementing an instance of 'FromJSON'
--   from a 'ByteString'.
--
-- Returns 'Nothing' if reading fails.
readFromByteStringAsJSON :: FromJSON a => ByteString -> Maybe a
readFromByteStringAsJSON = fromByteString (Nothing :: Maybe (ReadableAsJSON a))

-- | Reads a pointer to the Nginx request object followed by an object of
--   a custom type implementing an instance of 'Read' from a 'ByteString'.
--
-- Throws an exception if unmarshalling of the request pointer fails. In the
-- second element of the tuple returns 'Nothing' if reading of the custom
-- object fails. Notice that the value of the returned request pointer is not
-- checked against /NULL/.
readFromByteStringWithRPtr :: Read a => ByteString -> (Ptr (), Maybe a)
readFromByteStringWithRPtr = ngxRequestPtr &&& readFromByteString . skipRPtr

-- | Reads a pointer to the Nginx request object followed by an object of
--   a custom type implementing an instance of 'FromJSON' from a 'ByteString'.
--
-- Throws an exception if unmarshalling of the request pointer fails. In the
-- second element of the tuple returns 'Nothing' if decoding of the custom
-- object fails. Notice that the value of the returned request pointer is not
-- checked against /NULL/.
readFromByteStringWithRPtrAsJSON :: FromJSON a =>
    ByteString -> (Ptr (), Maybe a)
readFromByteStringWithRPtrAsJSON =
    ngxRequestPtr &&& readFromByteStringAsJSON . skipRPtr

skipRPtr :: ByteString -> ByteString
skipRPtr = B.drop $ sizeOf (undefined :: Word)

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
-- ==== File /test_tools.hs/
-- @
-- {-\# LANGUAGE TemplateHaskell, DeriveGeneric, RecordWildCards \#-}
--
-- module TestTools where
--
-- import           NgxExport
-- import           NgxExport.Tools
--
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString.Lazy.Char8 as C8L
-- import           Data.Aeson
-- import           Data.IORef
-- import           Control.Monad
-- import           GHC.Generics
--
-- test :: ByteString -> Bool -> IO L.ByteString
-- __/test/__ = const . return . L.fromStrict
-- 'ngxExportSimpleService' \'test $
--     'PersistentService' $ Just $ 'Sec' 10
--
-- showAsLazyByteString :: Show a => a -> L.ByteString
-- showAsLazyByteString = C8L.pack . show
--
-- testRead :: Show a => a -> IO L.ByteString
-- testRead = return . showAsLazyByteString
--
-- testReadInt :: Int -> Bool -> IO L.ByteString
-- __/testReadInt/__ = const . testRead
-- 'ngxExportSimpleServiceTyped' \'testReadInt \'\'Int $
--     'PersistentService' $ Just $ 'Sec' 10
--
-- newtype Conf = Conf Int deriving (Read, Show)
--
-- testReadConf :: Conf -> Bool -> IO L.ByteString
-- __/testReadConf/__ = const . testRead
-- 'ngxExportSimpleServiceTyped' \'testReadConf \'\'Conf $
--     'PersistentService' $ Just $ 'Sec' 10
--
-- testConfStorage :: ByteString -> IO L.ByteString
-- __/testConfStorage/__ = const $
--     showAsLazyByteString \<$\> readIORef __/storage_Conf_/__testReadConf
-- 'ngxExportIOYY' \'testConfStorage
--
-- data ConfWithDelay = ConfWithDelay { delay :: 'TimeInterval'
--                                    , value :: Int
--                                    } deriving (Read, Show)
--
-- testReadConfWithDelay :: ConfWithDelay -> Bool -> IO L.ByteString
-- __/testReadConfWithDelay/__ c\@ConfWithDelay {..} fstRun = do
--     unless fstRun $ 'threadDelaySec' $ 'toSec' delay
--     testRead c
-- 'ngxExportSimpleServiceTyped' \'testReadConfWithDelay \'\'ConfWithDelay $
--     'PersistentService' Nothing
--
-- data ConfJSON = ConfJSONCon1 Int
--               | ConfJSONCon2 deriving (Generic, Show)
-- instance FromJSON ConfJSON
--
-- testReadConfJSON :: ConfJSON -> Bool -> IO L.ByteString
-- __/testReadConfJSON/__ = 'ignitionService' testRead
-- 'ngxExportSimpleServiceTypedAsJSON' \'testReadConfJSON \'\'ConfJSON
--     'SingleShotService'
-- @
--
-- Here five simple services of various types are defined: /test/,
-- /testReadInt/, /testReadConf/, /testReadConfWithDelay/, and
-- /testReadConfJSON/. /Typed/ services hold 'IORef' /storages/ to save their
-- configurations for faster access in future iterations. The name of a storage
-- consists of the name of its type and the name of the service connected by an
-- underscore and prefixed as a whole word with __/storage_/__.
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
-- service because data is not altered during runtime. A single-shot service
-- runs exactly two times during the lifetime of a worker process: the first
-- run (when the second argument of the service, i.e. the /first-run/ flag, is
-- /True/) is immediately followed by the second run (when the /first-run/ flag
-- is /False/). On the second run the service handler is used as an exception
-- handler when the service is shutting down after the 'WorkerProcessIsExiting'
-- exception has been thrown. Accordingly, a single-shot handler can be used
-- for allocation of some global resources (when the first-run flag is /True/),
-- and cleaning them up (when the first-run flag is /False/).
--
-- Notice that service /testReadConfWithDelay/ manages time delays on its own,
-- therefore it uses /no-sleeps/ strategy @'PersistentService' Nothing@.
--
-- ==== File /nginx.conf/
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
--     server {
--         listen       8010;
--         server_name  main;
--         error_log    \/tmp\/nginx-test-haskell-error.log;
--         access_log   \/tmp\/nginx-test-haskell-access.log;
--
--         location \/ {
--             haskell_run __/testConfStorage/__ $hs_testConfStorage \'\';
--
--             echo \"Service variables:\";
--             echo \"  hs_test: $hs_test\";
--             echo \"  hs_testReadInt: $hs_testReadInt\";
--             echo \"  hs_testReadConf: $hs_testReadConf\";
--             echo \"  hs_testReadConfWithDelay: $hs_testReadConfWithDelay\";
--             echo \"  hs_testReadConfJSON: $hs_testReadConfJSON\";
--             echo \"Storages of service variables:\";
--             echo \"  hs_testConfStorage: $hs_testConfStorage\";
--         }
--     }
-- }
-- @
--
-- Notice that Haskel handlers defined in /test_tools.hs/ are referred from
-- the Nginx configuration file with prefix __/simpleService_/__.
--
-- ==== A simple test
-- > $ curl 'http://localhost:8010/'
-- > Service variables:
-- >   hs_test: test
-- >   hs_testReadInt: 5000000
-- >   hs_testReadConf: Conf 20
-- >   hs_testReadConfWithDelay: ConfWithDelay {delay = Sec 10, value = 12}
-- >   hs_testReadConfJSON: ConfJSONCon1 56
-- > Storages of service variables:
-- >   hs_testConfStorage: Just (Conf 20)

-- | Defines a sleeping strategy.
data ServiceMode
    -- | Persistent service (with or without periodical sleeps)
    = PersistentService (Maybe TimeInterval)
    -- | Single-shot service
    | SingleShotService

ngxExportSimpleService' :: Name -> Maybe (Name, Bool) -> ServiceMode -> Q [Dec]
ngxExportSimpleService' f c m = do
    confBs <- newName "confBs_"
    fstRun <- newName "fstRun_"
    let nameF = nameBase f
        nameSsf = mkName $ "simpleService_" ++ nameF
        hasConf = isJust c
        (sNameC, typeC, readConf, unreadableConfMsg) =
            if hasConf
                then let (tName, isJSON) = first nameBase $ fromJust c
                     in (mkName $ "storage_" ++ tName ++ '_' : nameF
                        -- FIXME: using base name of the type means that it is
                        -- not possible to pass here qualified types from
                        -- external modules. Using showName instead of nameBase
                        -- won't help, as it adds static qualified names like
                        -- GHC.Types.Int that can be unexpected in the context
                        -- of the user's module scope, instead of adding the
                        -- dynamic namespace (possibly not qualified) specified
                        -- in the import clause of the user's module.
                        ,conT $ mkName tName
                        ,if isJSON
                             then [|readFromByteStringAsJSON|]
                             else [|readFromByteString|]
                        ,"Configuration " ++ tName ++ " is not readable"
                        )
                else undefined
        initConf =
            let eConfBs = varE confBs
            in if hasConf
                   then let storage = varE sNameC
                        in [|readIORef $(storage) >>=
                                 maybe
                                     (do
                                          let conf_data__ =
                                                  $(readConf) $(eConfBs)
                                          when (isNothing conf_data__) $
                                              terminateWorkerProcess
                                                  unreadableConfMsg
                                          writeIORef $(storage) conf_data__
                                          return conf_data__
                                     ) (return . Just)
                           |]
                   else [|return $
                              fromByteString (Nothing :: Maybe ByteString)
                                  $(eConfBs)
                        |]
        (waitTime, runService) =
            let eF = varE f
                eFstRun = varE fstRun
                runPersistentService = [|flip $(eF) $(eFstRun)|]
            in case m of
                   PersistentService (Just t) ->
                       ([|const $ unless $(eFstRun) $ threadDelaySec $ toSec t|]
                       ,runPersistentService
                       )
                   PersistentService Nothing ->
                       ([|const $ return ()|]
                       ,runPersistentService
                       )
                   SingleShotService ->
                       ([|\conf_data__ -> unless $(eFstRun) $
                              handle
                                  (const $ void $ $(eF) conf_data__ False ::
                                      WorkerProcessIsExiting -> IO ()
                                  ) $ forever $ threadDelaySec $ toSec $ Hr 24
                        |]
                       ,[|\conf_data__ ->
                              if $(eFstRun)
                                  then $(eF) conf_data__ True
                                  else return L.empty
                        |]
                       )
    concat <$> sequence
        [sequence $
            (if hasConf
                 then [sigD sNameC [t|IORef (Maybe $(typeC))|]
                      ,funD sNameC
                          [clause []
                              (normalB [|unsafePerformIO $ newIORef Nothing|])
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
                    (normalB [|do
                                   conf_data_ <- fromJust <$> $(initConf)
                                   $(waitTime) conf_data_
                                   $(runService) conf_data_
                             |]
                    )
                    []
                ]
            ]
        ,ngxExportServiceIOYY nameSsf
        ]

-- | Exports a simple service of type
--
-- @
-- 'ByteString' -> 'Prelude.Bool' -> 'IO' 'L.ByteString'
-- @
--
-- with specified name and service mode.
ngxExportSimpleService :: Name         -- ^ Name of the service
                       -> ServiceMode  -- ^ Service mode
                       -> Q [Dec]
ngxExportSimpleService f =
    ngxExportSimpleService' f Nothing

-- | Exports a simple service of type
--
-- @
-- 'Read' a => a -> 'Prelude.Bool' -> 'IO' 'L.ByteString'
-- @
--
-- with specified name and service mode.
--
-- The service expects an object of a custom type implementing an instance of
-- 'Read' at its first argument. For the sake of efficiency, this object gets
-- deserialized into a global 'IORef' data storage on the first service run to
-- be further accessed directly from this storage. The storage can be accessed
-- from elsewhere by a name comprised of the name of the custom type and the
-- name of the service connected by an underscore and prefixed as a whole word
-- with __/storage_/__. The stored data is wrapped in 'Maybe' container.
--
-- When reading of the custom object fails on the first service run, the
-- service terminates the worker process by calling 'terminateWorkerProcess'
-- with a corresponding message.
ngxExportSimpleServiceTyped :: Name         -- ^ Name of the service
                            -> Name         -- ^ Name of the custom type
                            -> ServiceMode  -- ^ Service mode
                            -> Q [Dec]
ngxExportSimpleServiceTyped f c =
    ngxExportSimpleService' f $ Just (c, False)

-- | Exports a simple service of type
--
-- @
-- 'FromJSON' a => a -> 'Prelude.Bool' -> 'IO' 'L.ByteString'
-- @
--
-- with specified name and service mode.
--
-- The service expects an object of a custom type implementing an instance of
-- 'FromJSON' at its first argument. For the sake of efficiency, this object
-- gets deserialized into a global 'IORef' data storage on the first service
-- run to be further accessed directly from this storage. The storage can be
-- accessed from elsewhere by a name comprised of the name of the custom type
-- and the name of the service connected by an underscore and prefixed as a
-- whole word with __/storage_/__. The stored data is wrapped in 'Maybe'
-- container.
--
-- When reading of the custom object fails on the first service run, the
-- service terminates the worker process by calling 'terminateWorkerProcess'
-- with a corresponding message.
ngxExportSimpleServiceTypedAsJSON :: Name         -- ^ Name of the service
                                  -> Name         -- ^ Name of the custom type
                                  -> ServiceMode  -- ^ Service mode
                                  -> Q [Dec]
ngxExportSimpleServiceTypedAsJSON f c =
    ngxExportSimpleService' f $ Just (c, True)

-- $splitServices
--
-- Here are a number of combinators to facilitate creation of specialized
-- services. They allow distinguishing between /ignition/ and /deferred/
-- services: the former run when the /first-run/ flag is /True/ whereas the
-- latter run when the flag is /False/. The most promising use case for these
-- helper functions is tuning of /single-shot/ services: in this case the
-- ignition service corresponds to a normal single-shot action on startup of a
-- worker process, while the deferred service corresponds to a cleanup handler
-- and runs when a worker process exits.
--
-- In all helpers, configuration and the first-run flag parameters belong to
-- the common service signature, and therefore should not be bound by any
-- arguments.

-- | Sets two different actions as ignition and deferred services.
--
-- When used as a single-shot service, the second action only runs on exit of a
-- worker process, and therefore can be used as a cleanup handler.
splitService :: (a -> IO L.ByteString)  -- ^ Ignition service
             -> (a -> IO L.ByteString)  -- ^ Deferred service
             -> a                       -- ^ Configuration
             -> Bool                    -- ^ First-run flag
             -> IO L.ByteString
splitService is ds c fstRun
    | fstRun = is c
    | otherwise = ds c

-- | Sets an action as an ignition service.
ignitionService :: (a -> IO L.ByteString)  -- ^ Ignition service
                -> a                       -- ^ Configuration
                -> Bool                    -- ^ First-run flag
                -> IO L.ByteString
ignitionService is = splitService is $ const $ return L.empty

-- | Sets an action as a deferred service.
--
-- When used as a single-shot service, the action only runs on exit of a worker
-- process, and therefore can be used as a cleanup handler.
deferredService :: (a -> IO L.ByteString)  -- ^ Deferred service
                -> a                       -- ^ Configuration
                -> Bool                    -- ^ First-run flag
                -> IO L.ByteString
deferredService = splitService $ const $ return L.empty

