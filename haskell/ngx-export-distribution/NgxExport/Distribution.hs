{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Distribution
-- Copyright   :  (c) Alexey Radkov 2021
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Quick and dirty build of simple shared libraries and collecting
-- dependencies. This was designed to build custom Haskell handlers for
-- <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------


module NgxExport.Distribution (
    -- * Usage and examples
    -- $usage
                               buildSharedLib
                              ,patchAndCollectDependentLibs
                              ,ngxExportHooks
                              ,defaultMain
                              ) where

import Distribution.Simple hiding (defaultMain)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Program.Run
import Distribution.Simple.Program.Db
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Types.PackageDescription
import Distribution.Types.BuildInfo
import Distribution.Types.Library
import Distribution.Verbosity
import Distribution.Pretty
import System.Directory
import System.FilePath
import Control.Arrow
import Control.Monad
import Data.Maybe

-- $usage
--
-- This module allows for building simple shared libraries with Cabal.
--
-- Below is a simple example.
--
-- ==== File /ngx_distribution_test.hs/
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
--
-- module NgxDistributionTest where
--
-- import           NgxExport
--
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Lazy.Char8 as C8L
-- import           Data.Aeson
-- import           Data.Maybe
--
-- incCnt :: ByteString -> C8L.ByteString
-- incCnt = C8L.pack . show . succ . fromMaybe (0 :: Int) . decodeStrict
-- ngxExportYY \'incCnt
-- @
--
-- ==== File /ngx-distribution-test.cabal/
-- @
-- name:                       ngx-distribution-test
-- version:                    0.1.0.0
-- build-type:                 __/Custom/__
-- cabal-version:              1.24
--
-- __/custom-setup/__
--   setup-depends:            base >= 4.8 && < 5
--                           , __/ngx-export-distribution/__
--
-- library
--   default-language:         Haskell2010
--   build-depends:            base >= 4.8 && < 5
--                           , ngx-export
--                           , bytestring
--                           , aeson
--
--   ghc-options:             -Wall -O2 -no-keep-hi-files -no-keep-o-files
-- @
--
-- ==== File /Setup.hs/
-- @
-- import __/NgxExport.Distribution/__
-- main = 'defaultMain'
-- @
--
-- The configuration step requires that utilities /patchelf/ and
-- <https://github.com/lyokha/nginx-haskell-module/blob/master/utils/hslibdeps hslibdeps>
-- were found in the paths of environment variable /$PATH/.
--
-- Building is a bit cumbersome: it expects explicit option /--prefix/ at the
-- configuration step (which will be interpreted as the prefix part of the
-- /rpath/ by utility /hslibdeps/) and explicit ghc option /-o/ at the build
-- step which is as well used by /hslibdeps/ as the name of the target library.
--
-- Let's build the example with commands /cabal v1-configure/ and
-- /cabal v1-build/ (the /v2-/commands should probably work as well).
--
-- > $ cabal v1-configure --prefix=/var/lib/nginx
-- > Resolving dependencies...
-- > [1 of 1] Compiling Main             ( dist/setup/setup.hs, dist/setup/Main.o )
-- > Linking ./dist/setup/setup ...
-- > Configuring ngx-distribution-test-0.1.0.0...
--
-- > $ cabal v1-build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so"
-- > [1 of 1] Compiling NgxDistributionTest ( ngx_distribution_test.hs, ngx_distribution_test.o )
-- > Linking ngx_distribution_test.so ...
-- > ---> Collecting libraries
-- > '/usr/lib64/libHSrts-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSrts-ghc8.10.5.so'
-- > '/home/lyokha/.cabal/lib/x86_64-linux-ghc-8.10.5/libHSngx-export-1.7.5-JzTEmHewqdC9gGi6rzcAtt-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSngx-export-1.7.5-JzTEmHewqdC9gGi6rzcAtt-ghc8.10.5.so'
-- > '/home/lyokha/.cabal/lib/x86_64-linux-ghc-8.10.5/libHSmonad-loops-0.4.3-8Lx5Hn3pTtO62yOPdPW77x-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSmonad-loops-0.4.3-8Lx5Hn3pTtO62yOPdPW77x-ghc8.10.5.so'
-- > '/home/lyokha/.cabal/lib/x86_64-linux-ghc-8.10.5/libHSasync-2.2.4-ENjuIeC23kaKyMVDRYThP3-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSasync-2.2.4-ENjuIeC23kaKyMVDRYThP3-ghc8.10.5.so'
-- > '/usr/lib64/libHSstm-2.5.0.1-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSstm-2.5.0.1-ghc8.10.5.so'
-- > '/home/lyokha/.cabal/lib/x86_64-linux-ghc-8.10.5/libHSaeson-1.5.6.0-6XeGmWHoO3vJYEUW5PXPgC-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSaeson-1.5.6.0-6XeGmWHoO3vJYEUW5PXPgC-ghc8.10.5.so'
-- >
-- >    ...
-- >
-- > '/usr/lib64/libHSbase-4.14.2.0-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSbase-4.14.2.0-ghc8.10.5.so'
-- > '/usr/lib64/libHSinteger-gmp-1.0.3.0-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSinteger-gmp-1.0.3.0-ghc8.10.5.so'
-- > '/usr/lib64/libHSghc-prim-0.6.1-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSghc-prim-0.6.1-ghc8.10.5.so'
-- >
-- > ---> Patching ngx_distribution_test.so
-- > /var/lib/nginx/x86_64-linux-ghc-8.10.5:/home/lyokha/.cabal/lib/x86_64-linux-ghc-8.10.5:/usr/lib64:/usr/lib64/ghc-8.10.5/rts
--
-- Now the current working directory contains new files
-- /ngx_distribution_test.so/ and /ngx-distribution-test-0.1.0.0.tar.gz/ and a
-- new directory /x86_64-linux-ghc-8.10.5/. The tar-file contains the patched
-- shared library and the directory with dependent libraries: it is ready for
-- installation in directory /\/var\/lib\/nginx/ at the target system.
--
-- With this building approach, the following list of drawbacks must be taken
-- into account.
--
-- - Utility /hslibdeps/ collects only libraries prefixed with /libHS/,
-- - command /cabal v1-clean/ only deletes directory /dist/, it does not delete
--   build artifacts in the current working directory,
-- - behavior of Cabal commands other than /configure/, /build/ and /clean/ is
--   not well defined.

hslibdeps :: Program
hslibdeps = simpleProgram "hslibdeps"

patchelf :: Program
patchelf = simpleProgram "patchelf"

-- | Builds a shared library.
--
-- Runs /ghc/ compiler with the following arguments.
--
-- - /-dynamic/, /-shared/, /-fPIC/, /-flink-rts/,
-- - all arguments listed in /ghc-options/ in the Cabal file,
-- - all arguments passed in option /--ghc-options/ from command-line,
-- - if arguments do not contain /-o path/ so far, then /$pkg.hs/, /-o $pkg.so/.
--
-- Returns the path to the built shared library.
buildSharedLib :: Verbosity                         -- ^ Verbosity level
               -> PackageDescription                -- ^ Package description
               -> LocalBuildInfo                    -- ^ Local build info
               -> BuildFlags                        -- ^ Build flags
               -> IO FilePath
buildSharedLib verbosity desc lbi flags = do
    let configGhcOptions =
            maybe [] (map ("ghc", )) $
                lookup GHC $ perCompilerFlavorToList $
                    options $ libBuildInfo $ fromJust $ library desc
        lib = fst $
            foldl (\a@(r, _) (prog, v) ->
                if prog /= "ghc" || isJust r
                    then a
                    else foldl (\a'@(r', ready) v' ->
                                    if isJust r'
                                        then a'
                                        else if v' == "-o"
                                                 then (Nothing, True)
                                                 else if ready
                                                          then (Just v', False)
                                                          else (Nothing, False)
                               ) a v
                  ) (Nothing, False) $
                      buildProgramArgs flags ++
                          map (second pure) configGhcOptions
        (lib', extraGhcOptions) =
            maybe (let name = unPackageName $ pkgName $ package desc
                       nameSo = addExtension name "so"
                   in (nameSo, [addExtension name "hs", "-o", nameSo])
                  ) (, []) lib
    (extraSourceFile, extraSourceFileDoesNotExist) <-
        if null extraGhcOptions
            then return (Nothing, False)
            else do
                let file = head extraGhcOptions
                (Just file, ) . not <$> doesFileExist file
    when extraSourceFileDoesNotExist $
        ioError $ userError $
            "File " ++ fromJust extraSourceFile ++ " does not exist,\
            \ you may want to specify input and output files in --ghc-options"
    ghcP <- fst <$> requireProgram verbosity ghcProgram (withPrograms lbi)
    let ghcR = programInvocation ghcP $
            ["-dynamic", "-shared", "-fPIC", "-flink-rts"] ++
                map snd configGhcOptions ++ extraGhcOptions
    runProgramInvocation verbosity ghcR
    return lib'

-- | Patches the shared library and collects dependent Haskell libraries.
--
-- Performs the following steps.
--
-- - Adds the value of /prefix/ to the list of /rpath/ in the shared library,
-- - collects all dependent Haskell libraries in a directory whose name forms
--   as /arch-os-compiler/,
-- - archives the shared library and the directory with the collected dependent
--   libraries in a /tar.gz/ file.
--
-- The initial 2 steps are performed by utility
-- <https://github.com/lyokha/nginx-haskell-module/blob/master/utils/hslibdeps hslibdeps>.
-- It collects all libraries with prefix /libHS/ from the list returned by
-- command /ldd/ applied to the shared library.
patchAndCollectDependentLibs :: Verbosity           -- ^ Verbosity level
                             -> FilePath            -- ^ Path to the library
                             -> PackageDescription  -- ^ Package description
                             -> LocalBuildInfo      -- ^ Local build info
                             -> IO ()
patchAndCollectDependentLibs verbosity lib desc lbi = do
    let dir = maybe "unspecified-abi" fromPathTemplate $ lookup AbiVar $
            abiTemplateEnv (compilerInfo $ compiler lbi) $ hostPlatform lbi
        dirArg = "-d" : [dir]
        rpathArg = maybe [] (("-t" :) . pure . (</> dir) . fromPathTemplate) $
            flagToMaybe $ prefix $ configInstallDirs $ configFlags lbi
        plbi = withPrograms lbi
    hslibdepsP <- fst <$> requireProgram verbosity hslibdeps plbi
    let hslibdepsR = programInvocation hslibdepsP $ lib : rpathArg ++ dirArg
    runProgramInvocation verbosity hslibdepsR
    tarP <- fst <$> requireProgram verbosity tarProgram plbi
    let tar = addExtension (prettyShow $ package desc) "tar.gz"
        tarR = programInvocation tarP ["czf", tar, lib, dir]
    runProgramInvocation verbosity tarR

-- | Build hooks.
--
-- Based on 'simpleUserHooks'. Overrides
--
-- - 'confHook' by configuring programs /hslibdeps/ and /patchelf/ and then
--   running the original /confHook/ from /simpleUserHooks/,
-- - 'buildHook' by running in sequence 'buildSharedLib' and
--   'patchAndCollectDependentLibs'.
--
-- Other hooks from /simpleUserHooks/ get derived as is. Running them is
-- neither tested nor recommended.
ngxExportHooks :: Verbosity                         -- ^ Verbosity level
               -> UserHooks
ngxExportHooks verbosity =
    let hooks = simpleUserHooks
    in hooks { hookedPrograms = [hslibdeps]
             , confHook = \desc flags -> do
                 let pdb = configPrograms flags
                 _ <- requireProgram verbosity hslibdeps pdb >>=
                          requireProgram verbosity patchelf . snd
                 confHook simpleUserHooks desc flags
             , buildHook =  \desc lbi _ flags ->
                 buildSharedLib verbosity desc lbi flags >>= \lib ->
                     patchAndCollectDependentLibs verbosity lib desc lbi
             }

-- | A simple implementation of /main/ for a Cabal setup script.
--
-- Implemented as
--
-- @
-- defaultMain = 'defaultMainWithHooks' $ 'ngxExportHooks' 'normal'
-- @
defaultMain :: IO ()
defaultMain = defaultMainWithHooks $ ngxExportHooks normal

