-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.Combinators
-- Copyright   :  (c) Alexey Radkov 2023
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
    -- * Split services
                                   ,module NgxExport.Tools.SplitService
                                   ) where

import           NgxExport.Tools.SplitService

import qualified Data.ByteString.Lazy as L

-- $description
--
-- A set of functions to combine effectful actions for building handlers and
-- services tuned for special purposes.

-- | Runs an effectful computation and then returns an empty 'L.ByteString'
--
-- This function saves printing the final @return L.empty@ action in handlers
-- that return unused or empty 'L.ByteString'.
--
-- For example, handler /signalUpconf/ being declared as an
-- [/update callback/](https://github.com/lyokha/nginx-haskell-module#update-callbacks)
-- in
--
-- @
-- type Upconf = [Text]
--
-- signalUpconf :: Upconf -> Bool -> IO L.ByteString
-- signalUpconf upconf = const $ do
--     mapConcurrently_ getUrl upconf
--     return L.empty
--
-- 'NgxExport.Tools.SimpleService.ngxExportSimpleServiceTyped' \'signalUpconf \'\'Upconf $
--     'NgxExport.Tools.SimpleService.PersistentService' Nothing
-- @
--
-- returns an empty bytestring which is not used in a meaningful way, therefore
-- it can be rewritten as
--
-- @
-- signalUpconf :: Upconf -> Bool -> IO L.ByteString
-- signalUpconf = const . __/voidHandler/__ . mapConcurrently_ getUrl
-- @
--
-- which helps to focus better on the computation itself.
--
-- @since 1.2.0
voidHandler :: IO a                         -- ^ Target computation
            -> IO L.ByteString
voidHandler = (>> return L.empty)

-- | Runs an effectful computation and then returns an empty 'L.ByteString'
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
-- For instance, handler /signalUpconf/ from the example for 'voidHandler' can
-- be further simplified as
--
-- @
-- signalUpconf :: Upconf -> Bool -> IO L.ByteString
-- signalUpconf = __/voidHandler'/__ . mapConcurrently_ getUrl
-- @
--
-- @since 1.2.1
voidHandler' :: IO a                        -- ^ Target computation
             -> b                           -- ^ Ignored value
             -> IO L.ByteString
voidHandler' = const . voidHandler

