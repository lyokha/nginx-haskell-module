{-# LANGUAGE TemplateHaskell, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.SimpleService
-- Copyright   :  (c) Alexey Radkov 2018-2024
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
-- Portability :  non-portable (requires Template Haskell)
--
-----------------------------------------------------------------------------


module NgxExport.Tools.SimpleService (
    -- * Exporters of simple services
    -- $description

    -- *** Preloading storages of persistent typed services
    -- $preload

    -- * Exported data and functions
                                      ServiceMode (..)
                                     ,ngxExportSimpleService
                                     ,ngxExportSimpleServiceTyped
                                     ,ngxExportSimpleServiceTypedAsJSON
                                     ,ngxExportSimpleServiceTyped'
                                     ,ngxExportSimpleServiceTypedAsJSON'
    -- * Type declarations
                                     ,NgxExportService
    -- * Re-exported data constructors from /Foreign.C/
    -- | Re-exports are needed by exporters for marshalling in foreign calls.
                                     ,Foreign.C.Types.CInt (..)
                                     ,Foreign.C.Types.CUInt (..)
                                     ) where

import           NgxExport
import           NgxExport.Tools.Read
import           NgxExport.Tools.System
import           NgxExport.Tools.TimeInterval
import           NgxExport.Tools.Types (NgxExportService)

import           Language.Haskell.TH
import           Foreign.C.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.IORef
import           Data.Maybe
import           Control.Monad
import           Control.Arrow
import           Control.Exception
import           System.IO.Unsafe (unsafePerformIO)

-- $description
--
-- This module implements a number of exporters for /simple services/. Here
-- /simplicity/ means avoiding boilerplate code regarding to efficient reading
-- of typed configurations and timed restarts of services.
--
-- All simple services are classified as /untyped/ or /typed/. The untyped
-- services have type
--
-- @
-- t'ByteString' -> 'Prelude.Bool' -> 'IO' t'L.ByteString'
-- @
--
-- which corresponds to the type of usual services from module "NgxExport". The
-- typed services are backed by functions from module "NgxExport.Tools.Read"
-- and may have two different types:
--
-- @
-- 'Read' a => a -> 'Prelude.Bool' -> 'IO' t'L.ByteString'
-- @
-- @
-- t'Data.Aeson.FromJSON' a => a -> 'Prelude.Bool' -> 'IO' t'L.ByteString'
-- @
--
-- The choice of a certain type of a typed service depends on the format in
-- which the typed data will be passed from the Nginx configuration.
--
-- Below is a simple example.
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
--     showAsLazyByteString \<$\> readIORef __/storage_Conf_testReadConf/__
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
-- __/testReadConfJSON/__ = 'NgxExport.Tools.SplitService.ignitionService' testRead
-- 'ngxExportSimpleServiceTypedAsJSON' \'testReadConfJSON \'\'ConfJSON
--     'SingleShotService'
-- @
--
-- Here five simple services of various types are defined: /test/,
-- /testReadInt/, /testReadConf/, /testReadConfWithDelay/, and
-- /testReadConfJSON/. /Persistent typed/ services hold 'IORef' /storages/ to
-- save their configurations for faster access in future iterations. The name of
-- a storage consists of the name of its type and the name of the service
-- connected by an underscore and prefixed as a whole word with __/storage_/__.
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
-- In this contrived example, the most efficient sleeping strategy is a
-- single-shot service because data is not altered during the runtime. A
-- single-shot service runs exactly two times during the lifetime of a worker
-- process: the first run (when the second argument of the service, i.e. the
-- /first-run/ flag, is /True/) is immediately followed by the second run (when
-- the /first-run/ flag is /False/). On the second run the service handler is
-- used as an exception handler when the service is shutting down after the
-- 'WorkerProcessIsExiting' exception has been thrown. Accordingly, a
-- single-shot handler can be used for allocation of some global resources
-- (when the /first-run/ flag is /True/), and cleaning them up (when the
-- /first-run/ flag is /False/).
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
--
-- In this example, persistent typed services had a single instance running. But
-- if there had been multiple instances of a single persistent typed service, we
-- would have had a problem. Remember that the name of the configuration storage
-- is made up of the names of the type and the service. This means that multiple
-- instances of a single persistent typed service share a single configuration
-- in the runtime which is not what is normally expected. Exporters
-- 'ngxExportSimpleServiceTyped'' and 'ngxExportSimpleServiceTypedAsJSON'' do
-- not store configurations and are preferable in such cases.
--
-- $preload
--
-- Storages of persistent typed services can be preloaded /synchronously/ with
-- 'ngxExportInitHook'. This is useful if a storage gets accessed immediately
-- after the start of processing client requests in a request handler which
-- expects that the storage has already been initialized (for example, a request
-- handler may unpack the storage with 'fromJust' without checking errors).
--
-- ==== File /test_tools.hs/: preload storage_Int_testReadInt
-- @
-- import           System.Environment
--
-- -- ...
--
-- initTestReadInt :: IO ()
-- __/initTestReadInt/__ = do
--     _ : v : _ \<- dropWhile (\/= \"__/--testReadInt/__\") \<$\> 'System.Environment.getArgs'
--     let i = read v
--     i \`seq\` writeIORef __/storage_Int_testReadInt/__ (Just i)
-- 'ngxExportInitHook' \'initTestReadInt
-- @
--
-- Note that the preloaded value gets evaluated inside the hook to spot any
-- parse errors inplace before the start of processing client requests.
--
-- ==== File /nginx.conf/: read data for storage_Int_testReadInt
-- @
--     haskell program_options __/--testReadInt/__ 800;
--
--     # ...
--
--     haskell_run_service __/simpleService_testReadInt/__
--             $hs_testReadInt
--             __/-/__;
-- @
--
-- The preloaded value gets passed in directive /haskell program_options/. The
-- value in the service declaration can be replaced by any lexeme as it won't
-- be parsed. In this example, it was replaced by a dash.

-- | Defines a sleeping strategy.
data ServiceMode
    -- | Persistent service (with or without periodical sleeps)
    = PersistentService (Maybe TimeInterval)
    -- | Single-shot service
    | SingleShotService

type TypedConf = (Name, (Bool, Bool))  -- (tName, (isJSON, storeConf))

ngxExportSimpleService' :: Name -> Maybe TypedConf -> ServiceMode -> Q [Dec]
ngxExportSimpleService' f c m = do
    confBs <- newName "confBs_"
    let nameF = nameBase f
        eConfBs = varE confBs
        (makeStorage, initConf) =
            flip (maybe ([], [|return $ Just $(eConfBs)|])) c $ \conf ->
                let ((tName, tNameBase), (isJSON, storeConf)) =
                        first (id &&& nameBase) conf
                    storeConf' =
                        case m of
                            PersistentService _ -> storeConf
                            _ -> False
                    sName = mkName $ "storage_" ++ tNameBase ++ '_' : nameF
                    eStorage = varE sName
                    (readStorage, writeStorage) =
                        if storeConf'
                            then ([|readIORef $(eStorage)|]
                                 ,[|writeIORef $(eStorage)|]
                                 )
                            else ([|return Nothing|]
                                 ,[|const $ return ()|]
                                 )
                    readConf =
                        if isJSON
                            then [|readFromByteStringAsJSON|]
                            else [|readFromByteString|]
                    noReadConfMsg =
                        "Configuration " ++ tNameBase ++ " is not readable"
                in (if storeConf'
                        then [sigD sName [t|IORef (Maybe $(conT tName))|]
                             ,funD sName
                                 [clause []
                                     (normalB
                                         [|unsafePerformIO $ newIORef Nothing|]
                                     )
                                     []
                                 ]
                             ,pragInlD sName NoInline FunLike AllPhases
                             ]
                        else []
                   ,[|$(readStorage) >>=
                          maybe (do
                                     let conf_data__ = $(readConf) $(eConfBs)
                                     when (isNothing conf_data__) $
                                         terminateWorkerProcess noReadConfMsg
                                     $(writeStorage) conf_data__
                                     return conf_data__
                                ) (return . Just)
                    |]
                   )
    fstRun <- newName "fstRun_"
    let nameSsf = mkName $ "simpleService_" ++ nameF
        eF = varE f
        eFstRun = varE fstRun
        runPersistentService = [|flip $(eF) $(eFstRun)|]
        (waitTime, runService) =
            case m of
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
            makeStorage ++
            [sigD nameSsf [t|ByteString -> Bool -> IO L.ByteString|]
            ,funD nameSsf
                [clause [varP confBs, varP fstRun]
                    (normalB
                        [|do
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
-- t'ByteString' -> 'Prelude.Bool' -> 'IO' t'L.ByteString'
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
-- 'Read' a => a -> 'Prelude.Bool' -> 'IO' t'L.ByteString'
-- @
--
-- with specified name and service mode.
--
-- The service expects an object of a custom type implementing an instance of
-- 'Read' at its first argument. For the sake of efficiency, when the service
-- mode is a 'PersistentService', this object gets deserialized into a global
-- 'IORef' data storage on the first service run to be further accessed directly
-- from this storage. The storage can be accessed from elsewhere by a name
-- comprised of the name of the custom type and the name of the service
-- connected by an underscore and prefixed as a whole word with __/storage_/__.
-- The stored data is wrapped in a 'Maybe' container which contains 'Nothing'
-- until the initialization on the first service run.
--
-- When reading of the custom object fails on the first service run, the
-- service terminates the worker process by calling 'terminateWorkerProcess'
-- with a corresponding message.
ngxExportSimpleServiceTyped :: Name         -- ^ Name of the service
                            -> Name         -- ^ Name of the custom type
                            -> ServiceMode  -- ^ Service mode
                            -> Q [Dec]
ngxExportSimpleServiceTyped f c =
    ngxExportSimpleService' f $ Just (c, (False, True))

-- | Exports a simple service of type
--
-- @
-- t'Data.Aeson.FromJSON' a => a -> 'Prelude.Bool' -> 'IO' t'L.ByteString'
-- @
--
-- with specified name and service mode.
--
-- The service expects an object of a custom type implementing an instance of
-- t'Data.Aeson.FromJSON' at its first argument. For the sake of efficiency,
-- when the service mode is a 'PersistentService', this object gets deserialized
-- into a global 'IORef' data storage on the first service run to be further
-- accessed directly from this storage. The storage can be accessed from
-- elsewhere by a name comprised of the name of the custom type and the name of
-- the service connected by an underscore and prefixed as a whole word with
-- __/storage_/__. The stored data is wrapped in a 'Maybe' container which
-- contains 'Nothing' until the initialization on the first service run.
--
-- When reading of the custom object fails on the first service run, the
-- service terminates the worker process by calling 'terminateWorkerProcess'
-- with a corresponding message.
ngxExportSimpleServiceTypedAsJSON :: Name         -- ^ Name of the service
                                  -> Name         -- ^ Name of the custom type
                                  -> ServiceMode  -- ^ Service mode
                                  -> Q [Dec]
ngxExportSimpleServiceTypedAsJSON f c =
    ngxExportSimpleService' f $ Just (c, (True, True))

-- | Exports a simple service of type
--
-- @
-- 'Read' a => a -> 'Prelude.Bool' -> 'IO' t'L.ByteString'
-- @
--
-- with specified name and service mode.
--
-- This exporter is similar to 'ngxExportSimpleServiceTyped' except it does not
-- store data in a global storage for persistent services. Use this exporter
-- when multiple instances of the service with different configurations are
-- required.
--
-- @since 1.2.5
ngxExportSimpleServiceTyped' :: Name         -- ^ Name of the service
                             -> Name         -- ^ Name of the custom type
                             -> ServiceMode  -- ^ Service mode
                             -> Q [Dec]
ngxExportSimpleServiceTyped' f c =
    ngxExportSimpleService' f $ Just (c, (False, False))

-- | Exports a simple service of type
--
-- @
-- t'Data.Aeson.FromJSON' a => a -> 'Prelude.Bool' -> 'IO' t'L.ByteString'
-- @
--
-- with specified name and service mode.
--
-- This exporter is similar to 'ngxExportSimpleServiceTypedAsJSON' except it
-- does not store data in a global storage for persistent services. Use this
-- exporter when multiple instances of the service with different configurations
-- are required.
--
-- @since 1.2.5
ngxExportSimpleServiceTypedAsJSON' :: Name         -- ^ Name of the service
                                   -> Name         -- ^ Name of the custom type
                                   -> ServiceMode  -- ^ Service mode
                                   -> Q [Dec]
ngxExportSimpleServiceTypedAsJSON' f c =
    ngxExportSimpleService' f $ Just (c, (True, False))

