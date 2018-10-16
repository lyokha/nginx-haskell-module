{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface, TypeFamilies #-}
{-# LANGUAGE DeriveGeneric, DeriveLift, NumDecimals #-}

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
                       ,ngxNow
                       ,threadDelaySec
                       ,TimeInterval (..)
                       ,toSec
    -- * Exporters of simple services
    --
    -- $simpleServices
                       ,ServiceMode (..)
                       ,ngxExportSimpleService
                       ,ngxExportSimpleServiceTyped
                       ,ngxExportSimpleServiceTypedAsJSON
    -- * Re-exported functions needed for building simple services
                       ,unsafePerformIO
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

-- | Delays current thread for the specified number of seconds.
threadDelaySec :: Int -> IO ()
threadDelaySec = threadDelay . (* 1e6)

-- | Time intervals.
data TimeInterval = Hr Int          -- ^ Hours
                  | Min Int         -- ^ Minutes
                  | Sec Int         -- ^ Seconds
                  | HrMin Int Int   -- ^ Hours and minutes
                  | MinSec Int Int  -- ^ Minutes and seconds
                  deriving (Generic, Lift, Read)
instance FromJSON TimeInterval

-- | Converts a time interval into seconds.
toSec :: TimeInterval -> Int
toSec (Hr h)       = 3600 * h
toSec (Min m)      = 60 * m
toSec (Sec s)      = s
toSec (HrMin h m)  = 3600 * h + 60 * m
toSec (MinSec m s) = 60 * m + s

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
-- {\-\# LANGUAGE TemplateHaskell, DeriveGeneric \#\-}
--
-- module TestTools where
--
-- import NgxExport
-- import NgxExport.Tools
--
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString.Lazy.Char8 as C8L
-- import           Data.Aeson
-- import           GHC.Generics
--
-- test :: ByteString -> Bool -> IO L.ByteString
-- __/test/__ = const . return . L.fromStrict
-- 'ngxExportSimpleService' \'test $
--     'PersistentService' $ Just $ 'Sec' 10
--
-- newtype ConfRead = ConfRead Int deriving (Read, Show)
--
-- testRead :: ConfRead -> Bool -> IO L.ByteString
-- __/testRead/__ c = const $ return $ C8L.pack $ show c
-- 'ngxExportSimpleServiceTyped' \'testRead \'\'ConfRead $
--     'PersistentService' $ Just $ 'Sec' 10
--
-- data ConfReadJSON = ConfReadJSONCon1 Int
--                   | ConfReadJSONCon2 deriving (Generic, Show)
-- instance FromJSON ConfReadJSON
--
-- testReadJSON :: ConfReadJSON -> Bool -> IO L.ByteString
-- __/testReadJSON/__ c = const $ return $ C8L.pack $ show c
-- 'ngxExportSimpleServiceTypedAsJSON' \'testReadJSON \'\'ConfReadJSON
--     'SingleShotService'
-- @
--
-- Here three simple services of various types are defined: /test/, /testRead/,
-- and /testReadJSON/. As soon as they merely echo their arguments into their
-- service variables, they must sleep for a while between iterations. Sleeps
-- are managed by strategies defined in type 'ServiceMode'. There are basically
-- three sleeping strategies:
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
-- All three services ignore their second parameter (of type 'Prelude.Bool')
-- denoting the first run of the service.
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
--     haskell_run_service __/simpleService_test/__ $hs_test
--             test;
--
--     haskell_run_service __/simpleService_testRead/__ $hs_testRead
--             \'ConfRead 20\';
--
--     haskell_run_service __/simpleService_testReadJSON/__ $hs_testReadJSON
--             \'{\"tag\":\"ConfReadJSONCon1\", \"contents\":56}\';
--
--     haskell_service_var_ignore_empty $hs_testReadJSON;
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
--             echo \"  hs_testRead: $hs_testRead\";
--             echo \"  hs_testReadJSON: $hs_testReadJSON\";
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
-- >   hs_testRead: ConfRead 20
-- >   hs_testReadJSON: ConfReadJSONCon1 56

newtype Readable a = Readable a
newtype ReadableAsJSON a = ReadableAsJSON a

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
ngxExportSimpleService' f c m = concat <$> sequence
    [sequence $
        (if hasConf
             then [sigD nameC [t|IORef (Maybe $(conT nameConC))|]
                  ,funD nameC [clause []
                                  (normalB
                                      [|unsafePerformIO $ newIORef Nothing|]
                                  )
                                  []
                              ]
                  ,pragInlD nameC NoInline FunLike AllPhases
                  ]
             else []
        )
        ++
        [sigD nameSf [t|ByteString -> Bool -> IO L.ByteString|]
        ,funD nameSf
            [clause [[p|confBs_|], [p|fstRun_|]]
                (normalB
                    [|do
                          conf_data_ <- $(initConf)
                          $(waitTime)
                          $(serviceWrap)
                    |]
                )
                []
            ]
        ]
    ,ngxExportServiceIOYY nameSf
    ]
    where sfName   = "simpleService_" ++ nameBase f
          nameSf   = mkName sfName
          hasConf  = isJust c
          (cName, isJSON) = if hasConf
                                then let c' = fromJust c
                                     in ("storage_" ++ nameBase (fst c')
                                        , snd c'
                                        )
                                else ("dummy__", False)
          nameC    = mkName cName
          nameConC = mkName $ if hasConf
                                   then let c' = fromJust c
                                        in nameBase (fst c')
                                   else "Dummy__"
          confType = if isJSON
                         then [t|ReadableAsJSON|]
                         else [t|Readable|]
          initConf =
              if hasConf
                  then [|readIORef $(varE nameC) >>=
                             maybe (do
                                        let conf_data__ =
                                                fromByteString
                                                    (Nothing :: Maybe
                                                        ($(confType)
                                                         $(conT nameConC)
                                                        )
                                                    ) confBs_
                                        when (isNothing conf_data__)
                                            terminateWorkerProcess
                                        writeIORef $(varE nameC) conf_data__
                                        return conf_data__
                                   ) (return . Just)
                       |]
                  else [|do
                             let conf_data__ =
                                     fromByteString
                                         (Nothing :: Maybe ByteString) confBs_
                             when (isNothing conf_data__)
                                 terminateWorkerProcess
                             return conf_data__
                       |]
          (waitTime, serviceWrap) =
              case m of
                  PersistentService i ->
                      (if isJust i
                           then let t = fromJust i
                                in [|unless fstRun_ $
                                         threadDelaySec $ toSec t
                                   |]
                           else [|return ()|]
                      ,[|simpleServiceWrap
                             $(varE f) (fromJust conf_data_) fstRun_
                       |]
                      )
                  SingleShotService ->
                      ([|unless fstRun_ $
                             threadDelaySec $ toSec $ Hr 1
                       |]
                      ,[|if fstRun_
                             then simpleServiceWrap
                                      $(varE f) (fromJust conf_data_) fstRun_
                             else return L.empty
                       |]
                      )

-- | Exports a simple service with specified name and service mode.
--
-- The service expects a plain 'ByteString' as its first argument.
ngxExportSimpleService :: Name         -- ^ Name of the service
                       -> ServiceMode  -- ^ Service mode
                       -> Q [Dec]
ngxExportSimpleService f =
    ngxExportSimpleService' f Nothing

-- | Exports a simple service with specified name and service mode.
--
-- The service expects a custom type deriving 'Read' as its first argument.
-- For the sake of efficiency, the object of this custom type gets deserialized
-- into a global 'IORef' data storage on the first service run to be further
-- accessed directly from the storage.
ngxExportSimpleServiceTyped :: Name         -- ^ Name of the service
                            -> Name         -- ^ Name of the custom type
                            -> ServiceMode  -- ^ Service mode
                            -> Q [Dec]
ngxExportSimpleServiceTyped f c =
    ngxExportSimpleService' f $ Just (c, False)

-- | Exports a simple service with specified name and service mode.
--
-- The service expects a custom type deriving 'FromJSON' as its first argument.
-- For the sake of efficiency, the object of this custom type gets deserialized
-- into a global 'IORef' data storage on the first service run to be further
-- accessed directly from the storage.
ngxExportSimpleServiceTypedAsJSON :: Name         -- ^ Name of the service
                                  -> Name         -- ^ Name of the custom type
                                  -> ServiceMode  -- ^ Service mode
                                  -> Q [Dec]
ngxExportSimpleServiceTypedAsJSON f c =
    ngxExportSimpleService' f $ Just (c, True)

