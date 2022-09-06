-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.SplitService
-- Copyright   :  (c) Alexey Radkov 2018-2022
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- Split services split the whole service into two separate actions for the
-- first (/ignition/ service) and the following (/deferred/ service) runs.
--
-----------------------------------------------------------------------------


module NgxExport.Tools.SplitService (splitService
                                    ,ignitionService
                                    ,deferredService
                                    ) where

import qualified Data.ByteString.Lazy as L

-- | Sets two different actions as ignition and deferred services.
--
-- When used as a single-shot service (in terms of module
-- "NgxExport.Tools.SimpleService"), the second action only runs on exit of a
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
-- When used as a single-shot service (in terms of module
-- "NgxExport.Tools.SimpleService"), the action only runs on exit of a worker
-- process, and therefore can be used as a cleanup handler.
deferredService :: (a -> IO L.ByteString)  -- ^ Deferred service
                -> a                       -- ^ Configuration
                -> Bool                    -- ^ First-run flag
                -> IO L.ByteString
deferredService = splitService $ const $ return L.empty

