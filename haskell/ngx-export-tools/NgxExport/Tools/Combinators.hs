-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.Combinators
-- Copyright   :  (c) Alexey Radkov 2023-2024
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------


module NgxExport.Tools.Combinators (
    -- * Combinators of effectful actions
    -- $description

    -- * Exported functions
                                    voidHandler
                                   ,voidHandler'
                                   ,voidService
                                   ,rareService
                                   ,restartPromptly
    -- * Split services
                                   ,module NgxExport.Tools.SplitService
                                   ) where

import           NgxExport.Tools.SimpleService
import           NgxExport.Tools.SplitService
import           NgxExport.Tools.TimeInterval

import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Lazy (LazyByteString)
import           Control.Monad

-- $description
--
-- A set of functions to combine effectful actions for building handlers and
-- services tuned for special purposes.

-- | Runs an effectful computation and then returns an empty 'LazyByteString'.
--
-- This function saves printing the final @return L.empty@ action in handlers
-- that return unused or empty 'LazyByteString'.
--
-- For example, service /signalUpconf/ being used as an
-- [/update callback/](https://github.com/lyokha/nginx-haskell-module#update-callbacks)
-- in
--
-- @
-- type Upconf = [Text]
--
-- signalUpconf :: Upconf -> t'NgxExport.Tools.Types.NgxExportService'
-- signalUpconf upconf = const $ do
--     mapConcurrently_ getUrl upconf
--     return L.empty
--
-- 'ngxExportSimpleServiceTyped' \'signalUpconf \'\'Upconf $
--     'PersistentService' Nothing
-- @
--
-- returns an empty bytestring which is not used in a meaningful way, therefore
-- it can be rewritten as
--
-- @
-- signalUpconf :: Upconf -> t'NgxExport.Tools.Types.NgxExportService'
-- signalUpconf = const . __/voidHandler/__ . mapConcurrently_ getUrl
-- @
--
-- which helps to focus better on the computation itself.
--
-- @since 1.2.0
voidHandler :: IO a                         -- ^ Target computation
            -> IO LazyByteString
voidHandler = (>> return L.empty)

-- | Runs an effectful computation and then returns an empty 'LazyByteString'.
--
-- The same as 'voidHandler' except it accepts an additional value which is
-- ignored. Implemented as
--
-- @
-- voidHandler' = const . 'voidHandler'
-- @
--
-- This can be useful in declarations of services that accept a boolean flag
-- which marks whether the service is running for the first time. This flag is
-- often ignored though, in which case using @voidHandler'@ can simplify code.
--
-- For instance, service /signalUpconf/ from the example for 'voidHandler' can
-- be further simplified as
--
-- @
-- signalUpconf :: Upconf -> t'NgxExport.Tools.Types.NgxExportService'
-- signalUpconf = __/voidHandler'/__ . mapConcurrently_ getUrl
-- @
--
-- @since 1.2.1
voidHandler' :: IO a                        -- ^ Target computation
             -> b                           -- ^ Ignored value
             -> IO LazyByteString
voidHandler' = const . voidHandler

-- | A void service which does nothing and returns an empty 'LazyByteString'.
--
-- The service is implemented as a /split/ service in terms of module
-- "NgxExport.Tools.SplitService". On the first iteration the service returns
-- immediately, on the next iteration it sleeps until the worker process
-- terminates it during the shutdown.
--
-- This can be used for loading global data from the Nginx configuration in a
-- more concise and declarative way.
--
-- For example, if data /Conf/ in
--
-- @
-- newtype Conf = Conf Int deriving (Read, Show)
--
-- testLoadConf :: Conf -> t'NgxExport.Tools.Types.NgxExportService'
-- testLoadConf = __/voidService/__
--
-- 'ngxExportSimpleServiceTyped' \'testLoadConf \'\'Conf 'restartPromptly'
-- @
--
-- gets loaded by service /testLoadConf/ from the Nginx configuration, then it
-- can be accessed in the Haskell code via t'Data.IORef.IORef' data storage
-- /storage_Conf_testLoadConf/.
--
-- Declaration of 'restartPromptly' establishes a /persistent/ service mode
-- without delay. The short iteration at the start of the service can be used
-- for calling a /service update hook/.
--
-- Note that /voidService/ is still an /asynchronous/ service which means that
-- the global data it loads may appear uninitialized in very early client
-- requests. To ensure that the data gets loaded before processing client
-- requests, consider using the /synchronous/ initialization hook
-- 'NgxExport.ngxExportInitHook' as a distinct solution or in conjunction with
-- other services.
--
-- @since 1.2.3
voidService :: a                            -- ^ Ignored configuration
            -> Bool                         -- ^ Ignored boolean value
            -> IO LazyByteString
voidService = splitService (voidHandler' $ return ()) $
    voidHandler' $ forever $ threadDelaySec $ toSec $ Hr 24

-- | A persistent service which waits for 24 hours before restart.
--
-- This declaration had been recommended for using with 'voidService' until the
-- latter was reimplemented as a split service. Nevertheless, it still can be
-- used for this purpose.
--
-- @since 1.2.5
rareService :: ServiceMode
rareService = PersistentService $ Just $ Hr 24

-- | A persistent service which restarts without delay.
--
-- This convenient declaration can be used for loading global data from the
-- Nginx configuration with 'voidService'.
--
-- @since 1.2.6
restartPromptly :: ServiceMode
restartPromptly = PersistentService Nothing

