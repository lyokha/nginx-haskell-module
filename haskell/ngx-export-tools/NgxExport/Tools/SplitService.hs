-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.SplitService
-- Copyright   :  (c) Alexey Radkov 2018-2023
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------


module NgxExport.Tools.SplitService (
    -- * Split services
    -- $description

    -- * Exported functions
                                     splitService
                                    ,ignitionService
                                    ,deferredService
    -- * Type declarations
                                    ,NgxExportService
                                    ) where

import           NgxExport.Tools.Types (NgxExportService)

import qualified Data.ByteString.Lazy as L

-- $description
--
-- Split services split the whole service into two separate actions for the
-- first (/ignition/ service) and the following (/deferred/ service) runs.

-- | Sets two different actions as ignition and deferred services.
--
-- When used as a single-shot service (in terms of module
-- "NgxExport.Tools.SimpleService"), the second action only runs on exit of a
-- worker process, and therefore can be used as a cleanup handler.
splitService :: (a -> IO L.ByteString)  -- ^ Ignition service
             -> (a -> IO L.ByteString)  -- ^ Deferred service
             -> a                       -- ^ Configuration
             -> NgxExportService
splitService is ds c fstRun
    | fstRun = is c
    | otherwise = ds c

-- | Sets an action as an ignition service.
ignitionService :: (a -> IO L.ByteString)  -- ^ Ignition service
                -> a                       -- ^ Configuration
                -> NgxExportService
ignitionService is = splitService is $ const $ return L.empty

-- | Sets an action as a deferred service.
--
-- When used as a single-shot service (in terms of module
-- "NgxExport.Tools.SimpleService"), the action only runs on exit of a worker
-- process, and therefore can be used as a cleanup handler.
deferredService :: (a -> IO L.ByteString)  -- ^ Deferred service
                -> a                       -- ^ Configuration
                -> NgxExportService
deferredService = splitService $ const $ return L.empty

