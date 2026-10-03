{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Distribution.Compat.Verbosity
-- Copyright   :  (c) Alexey Radkov 2026
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility functions to convert between different Cabal verbosity object parts.
-- The functions are meant to be used internally and only exported for usage in
-- utility /nhm-tool/.
--
-----------------------------------------------------------------------------

module NgxExport.Distribution.Compat.Verbosity (
    -- * Compatibility layer between Cabal /3.16/ and /3.18/
                                                toVerbosityFlags
                                               ,toVerbosity
                                               ,extractVerbosityFlags
                                               ,defaultVerbosity
                                               ) where

import Distribution.Simple.Flag (Flag, fromFlagOrDefault)

#if MIN_VERSION_Cabal(3,18,0)
#define VERBOSITY_FLAGS VerbosityFlags
#define VERBOSITY_HANDLES VerbosityHandles
#define TO_VERBOSITY_FLAGS(verbosity) (verbosityFlags (verbosity))
#define TO_VERBOSITY(flags, handles) (Verbosity (flags) (handles))
import Distribution.Verbosity (Verbosity (..)
                              ,VerbosityFlags, verbosityFlags, normal
                              ,VerbosityHandles, defaultVerbosityHandles
                              )
defaultVerbosityHandles_ :: VERBOSITY_HANDLES
defaultVerbosityHandles_ = defaultVerbosityHandles
#else
#define VERBOSITY_FLAGS Verbosity
#define VERBOSITY_HANDLES ()
#define TO_VERBOSITY_FLAGS(verbosity) (verbosity)
#define TO_VERBOSITY(flags, handles) (flags)
import Distribution.Verbosity (Verbosity, normal)
#endif

-- | Verbosity conversions.
--
-- In Cabal /3.16/ and older returns the passed argument. In Cabal /3.18/ and
-- newer extracts /verbosity flags/ from 'Verbosity'.
toVerbosityFlags :: Verbosity -> VERBOSITY_FLAGS
toVerbosityFlags verbosity = TO_VERBOSITY_FLAGS(verbosity)
{-# ANN toVerbosityFlags "HLint: ignore Redundant bracket" #-}

-- | Verbosity conversions.
--
-- In Cabal /3.16/ and older returns the passed argument. In Cabal /3.18/ and
-- newer combines /verbosity flags/ and /default verbosity handles/ into
-- 'Verbosity'.
toVerbosity :: VERBOSITY_FLAGS -> Verbosity
toVerbosity flags = TO_VERBOSITY(flags, defaultVerbosityHandles_)
{-# ANN toVerbosity "HLint: ignore Redundant bracket" #-}

-- | Extracts /verbosity flags/ from the passed flag.
--
-- Returns the extracted /verbosity flags/ or 'normal' on failure.
extractVerbosityFlags :: Flag VERBOSITY_FLAGS -> VERBOSITY_FLAGS
extractVerbosityFlags = fromFlagOrDefault $ toVerbosityFlags defaultVerbosity

-- | Returns verbosity with default /verbosity flags/ and /verbosity handles/.
--
-- Default /verbosity flags/ is 'normal'.
defaultVerbosity :: Verbosity
defaultVerbosity = toVerbosity normal

