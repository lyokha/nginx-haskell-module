{-# LANGUAGE DeriveGeneric, DeriveLift, NumDecimals #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.TimeInterval
-- Copyright   :  (c) Alexey Radkov 2018-2022
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------


module NgxExport.Tools.TimeInterval (
    -- * A simple implementation of time intervals
    -- $description

    -- * Exported data and functions
                                     TimeInterval (..)
                                    ,toSec
                                    ,threadDelaySec
                                    ) where

import           Language.Haskell.TH.Syntax
import           Data.Aeson.Types
import           Data.Function (on)
import           Data.Ord (comparing)
import           Control.Concurrent
import           GHC.Generics

-- $description
--
-- A simple implementation of time intervals supposed for describing low
-- resolution timeouts.

-- | Time intervals.
--
-- Note that /Unset/ is a zero time interval which is equal to 0 seconds,
-- however it is expected to be used differently, for example to explicitly
-- express an intention to unset some timeout.
data TimeInterval = Hr Int          -- ^ Hours
                  | Min Int         -- ^ Minutes
                  | Sec Int         -- ^ Seconds
                  | HrMin Int Int   -- ^ Hours and minutes
                  | MinSec Int Int  -- ^ Minutes and seconds
                  | Unset           -- ^ Zero time interval
                  deriving (Generic, Lift, Read, Show)

instance FromJSON TimeInterval

instance Eq TimeInterval where
    (==) = (==) `on` toSec

instance Ord TimeInterval where
    compare = comparing toSec

-- | Converts a time interval into seconds.
toSec :: TimeInterval -> Int
toSec (Hr h)       = 3600 * h
toSec (Min m)      = 60 * m
toSec (Sec s)      = s
toSec (HrMin h m)  = 3600 * h + 60 * m
toSec (MinSec m s) = 60 * m + s
toSec Unset        = 0

-- | Delays the current thread by the specified number of seconds.
threadDelaySec :: Int -> IO ()
threadDelaySec = threadDelay . (* 1e6)

