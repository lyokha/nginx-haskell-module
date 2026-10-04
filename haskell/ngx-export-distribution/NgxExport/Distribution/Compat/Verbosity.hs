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
-- Utility functions to extract and combine different Cabal verbosity object
-- parts. The functions are meant to be used internally and only exported for
-- usage in utility /nhm-tool/.
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
#define TO_VERBOSITY_FLAGS(verbosity) (verbosityFlags (verbosity))
#define TO_VERBOSITY(flags, handles) (mkVerbosity (handles) (flags))
import Distribution.Verbosity (Verbosity, mkVerbosity
                              ,VerbosityFlags, verbosityFlags, normal
                              ,defaultVerbosityHandles
                              )
#else
#define VERBOSITY_FLAGS Verbosity
#define TO_VERBOSITY_FLAGS(verbosity) (verbosity)
#define TO_VERBOSITY(flags, handles) (flags)
import Distribution.Verbosity (Verbosity, normal)
#endif

-- | Extracts /verbosity flags/ from 'Verbosity'.
--
-- With Cabal /3.16/ and older it simply returns the passed argument.
toVerbosityFlags :: Verbosity -> VERBOSITY_FLAGS
toVerbosityFlags verbosity = TO_VERBOSITY_FLAGS(verbosity)
{-# ANN toVerbosityFlags "HLint: ignore Redundant bracket" #-}

-- | Builds 'Verbosity' from the passed /verbosity flags/ and default
--   /verbosity handles/.
--
-- With Cabal /3.16/ and older it simply returns the passed argument.
toVerbosity :: VERBOSITY_FLAGS -> Verbosity
toVerbosity flags = TO_VERBOSITY(flags, defaultVerbosityHandles)
{-# ANN toVerbosity "HLint: ignore Redundant bracket" #-}

-- | Extracts /verbosity flags/ from the passed flag.
--
-- Returns the extracted /verbosity flags/ or 'normal' on failure.
extractVerbosityFlags :: Flag VERBOSITY_FLAGS -> VERBOSITY_FLAGS
extractVerbosityFlags = fromFlagOrDefault $ toVerbosityFlags defaultVerbosity

-- | Builds verbosity from default /verbosity flags/ and /verbosity handles/.
--
-- With Cabal /3.16/ and older it simply returns 'normal'. With Cabal /3.18/
-- and newer it combines 'normal' and default /verbosity handles/ into
-- 'Verbosity'.
defaultVerbosity :: Verbosity
defaultVerbosity = toVerbosity normal

