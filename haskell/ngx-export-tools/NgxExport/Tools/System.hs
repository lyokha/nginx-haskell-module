-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.System
-- Copyright   :  (c) Alexey Radkov 2018-2022
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
-- Portability :  non-portable (requires POSIX)
--
-----------------------------------------------------------------------------


module NgxExport.Tools.System (
    -- * Various functions to access low-level Nginx API
    -- $description

    -- * Exported functions
                               terminateWorkerProcess
                              ,restartWorkerProcess
                              ,finalizeHTTPRequest
                              ,workerProcessIsExiting
                              ,ngxRequestPtr
                              ,ngxNow
                              ,ngxPid
                              ) where

import           NgxExport

import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.C.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Binary.Get
import           Data.Maybe
import           Control.Exception
import           System.Posix.Types

-- $description
--
-- Various functions to access low-level Nginx API, mostly wrappers around
-- corresponding functions and data from module "NgxExport".

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

-- | Checks that a generic exception is of type 'WorkerProcessIsExiting'.
--
-- This can be useful to check quickly in an exception handler whether a
-- Haskell service has been interrupted because the worker process is exiting.
workerProcessIsExiting :: SomeException -> Bool
workerProcessIsExiting e =
    isJust (fromException e :: Maybe WorkerProcessIsExiting)

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

