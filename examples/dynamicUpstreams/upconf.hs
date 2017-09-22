{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns, NumDecimals, ScopedTypeVariables #-}

{- ghc -O2 -dynamic -shared -fPIC -lHSrts_thr-ghc$(ghc --numeric-version) \
 - upconf.hs -o /var/lib/nginx/upconf.so -fforce-recomp
 -
 - for making eventlog:
 - ghc -O2 -dynamic -shared -fPIC \
 - -lHSrts_thr_debug-ghc$(ghc --numeric-version) \
 - upconf.hs -o /var/lib/nginx/upconf.so -fforce-recomp -eventlog
 -
 - and put in nginx.conf
 - haskell rts_options -l; -}

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

type CollectedData = Map Destination [Destination]

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
    new <- catchBadResponse $ (fromMaybe M.empty . decode) <$> query url addr
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

