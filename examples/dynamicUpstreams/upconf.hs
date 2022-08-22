{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns, NumDecimals, OverloadedStrings, RecordWildCards #-}

-- ghc -O2 -dynamic -shared -fPIC -flink-rts -threaded \
--   upconf.hs -o /var/lib/nginx/upconf.so -fforce-recomp
--
-- for making eventlog:
-- ghc -O2 -dynamic -shared -fPIC -flink-rts -threaded -debug \
--   upconf.hs -o /var/lib/nginx/upconf.so -fforce-recomp -eventlog
--
-- and put in nginx.conf lines (first in main clause, second in http clause)
-- working_directory /tmp;
-- haskell rts_options -l;

module NgxHaskellUserRuntime where

import           NgxExport
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Network.HTTP.Client
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           System.IO.Unsafe
import           Data.IORef
import           Data.Maybe
import           Data.Aeson
import           Safe

--import NgxExport.Healthcheck ()

type Url = String           -- normally must start with /
type Destination = String   -- IP address or domain name

data Conf = Conf { updateInterval    :: TimeInterval
                 , dataSourceAddr    :: (Url, Destination)
                 } deriving (Read)

newtype Upconf = Upconf { upconfAddr :: (Url, Destination)
                        } deriving (Read)

data TimeInterval = Hr Int
                  | Min Int
                  | Sec Int
                  | HrMin (Int, Int)
                  | MinSec (Int, Int)
                  deriving (Read)

data ServerData = ServerData { sAddr        :: Destination
                             , sWeight      :: Maybe Int
                             , sMaxFails    :: Maybe Int
                             , sFailTimeout :: Maybe Int
                             } deriving (Show, Eq)

instance FromJSON ServerData where
    parseJSON = withObject "server_options" $ \o -> do
        sAddr        <- o .:  "addr"
        sWeight      <- o .:? "weight"
        sMaxFails    <- o .:? "max_fails"
        sFailTimeout <- o .:? "fail_timeout"
        return ServerData {..}

instance ToJSON ServerData where
    toJSON ServerData {..} =
        object $ catMaybes [ pure $ "addr"   .=      sAddr
                           , ("weight"       .=) <$> sWeight
                           , ("max_fails"    .=) <$> sMaxFails
                           , ("fail_timeout" .=) <$> sFailTimeout
                           ]

type CollectedData = Map Destination [ServerData]

collectedData :: IORef CollectedData
collectedData = unsafePerformIO $ newIORef M.empty
{-# NOINLINE collectedData #-}

httpManager = unsafePerformIO $ newManager defaultManagerSettings
{-# NOINLINE httpManager #-}

getResponse url = fmap responseBody . (parseRequest url >>=)

getUrl url = getResponse url $ flip httpLbs httpManager

query = (getUrl .) . flip mkAddr
    where mkAddr = (("http://" ++) .) . (++)

getUpstreams (C8.unpack -> conf) firstRun = do
    let Conf (toSec -> upd) (url, addr) = readDef (Conf (Hr 24) ("", "")) conf
    unless firstRun $ threadDelaySec upd
    new <- catchBadResponse $ fromMaybe M.empty . decode <$> query url addr
    old <- readIORef collectedData
    if new == old
        then return C8L.empty
        else do
            writeIORef collectedData new
            return $ encode new
    where catchBadResponse =
              handle $ \(_ :: SomeException) -> readIORef collectedData
          threadDelaySec = threadDelay . (* 1e6)
          toSec (Hr h)          = 3600 * h
          toSec (Min m)         = 60 * m
          toSec (Sec s)         = s
          toSec (HrMin (h, m))  = 3600 * h + 60 * m
          toSec (MinSec (m, s)) = 60 * m + s
ngxExportServiceIOYY 'getUpstreams

signalUpconf (C8.unpack -> conf) = const $ do
    let Upconf (url, addr) = readDef (Upconf ("", "")) conf
    void $ handle (\(_ :: SomeException) -> return C8L.empty) $ query url addr
    return C8L.empty
ngxExportServiceIOYY 'signalUpconf

