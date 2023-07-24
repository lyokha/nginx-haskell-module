{-# LANGUAGE CPP, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Distribution
-- Copyright   :  (c) Alexey Radkov 2021-2023
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Quick and dirty approach to building regular shared libraries and collecting
-- dependencies. This module was designed to build custom Haskell handlers for
-- <https://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------


module NgxExport.Distribution (
    -- * Building regular shared libraries
    -- $building-shared-libraries

    -- *** An example
    -- $example

    -- *** Building with cabal v1-commands
    -- $cabal-v1

    -- *** Building with Setup.hs commands
    -- $setup-hs

    -- *** Building dependencies with cabal v2-build
    -- $deps-cabal-v2

    -- *** Drawbacks
    -- $drawbacks

    -- * Exported functions
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

-- $building-shared-libraries
--
-- This module allows for building regular shared libraries and collecting
-- Haskell libraries they depend on with Cabal.

-- $example
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

-- $cabal-v1
--
-- Let's build the example with commands /cabal v1-configure/ and
-- /cabal v1-build/.
--
-- > $ cabal v1-install --only-dependencies
-- > Resolving dependencies...
-- > All the requested packages are already installed:
-- > Use --reinstall if you want to reinstall anyway.
--
-- > $ cabal v1-configure --prefix=/var/lib/nginx
-- > Resolving dependencies...
-- > [1 of 2] Compiling Main             ( dist/setup/setup.hs, dist/setup/Main.o )
-- > [2 of 2] Linking ./dist/setup/setup
-- > Configuring ngx-distribution-test-0.1.0.0...
--
-- > $ cabal v1-build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so -threaded"
-- > [1 of 2] Compiling NgxDistributionTest ( ngx_distribution_test.hs, ngx_distribution_test.o )
-- > [2 of 2] Linking ngx_distribution_test.so ...
-- > ---> Collecting libraries
-- > '/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.4.1/libHSngx-export-1.7.5-FkCfFIq2kiq6MpFtZt6Wso-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSngx-export-1.7.5-FkCfFIq2kiq6MpFtZt6Wso-ghc9.4.1.so'
-- > '/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.4.1/libHSmonad-loops-0.4.3-5HNgusEuKV7E9KDl2xfIIb-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSmonad-loops-0.4.3-5HNgusEuKV7E9KDl2xfIIb-ghc9.4.1.so'
-- > '/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.4.1/libHSasync-2.2.4-BHmUTH2SmtgLLoxIOXNoMc-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSasync-2.2.4-BHmUTH2SmtgLLoxIOXNoMc-ghc9.4.1.so'
-- > '/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.4.1/libHSaeson-2.1.0.0-79sgaqQ0msAJaL6HuNRLaK-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSaeson-2.1.0.0-79sgaqQ0msAJaL6HuNRLaK-ghc9.4.1.so'
-- > '/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.4.1/libHSwitherable-0.4.2-1AWCu2zvFImLTaoXk8CRkT-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSwitherable-0.4.2-1AWCu2zvFImLTaoXk8CRkT-ghc9.4.1.so'
-- > '/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.4.1/libHSuuid-types-1.0.5-BduubbeXxFCF9me5IkbXLU-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSuuid-types-1.0.5-BduubbeXxFCF9me5IkbXLU-ghc9.4.1.so'
-- >
-- >    ...
-- >
-- > '/usr/lib64/ghc-9.4.1/lib/../lib/x86_64-linux-ghc-9.4.1/libHSghc-bignum-1.3-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSghc-bignum-1.3-ghc9.4.1.so'
-- > '/usr/lib64/ghc-9.4.1/lib/../lib/x86_64-linux-ghc-9.4.1/libHSghc-prim-0.9.0-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSghc-prim-0.9.0-ghc9.4.1.so'
-- > '/usr/lib64/ghc-9.4.1/lib/../lib/x86_64-linux-ghc-9.4.1/libHSrts-1.0.2_thr-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSrts-1.0.2_thr-ghc9.4.1.so'
-- >
-- > ---> Patching ngx_distribution_test.so
-- > /var/lib/nginx/x86_64-linux-ghc-9.4.1:/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.4.1:/usr/lib64/ghc-9.4.1/lib/../lib/x86_64-linux-ghc-9.4.1
-- >
-- > ---> Archiving artifacts
-- > ngx_distribution_test.so
-- > x86_64-linux-ghc-9.4.1/
-- > x86_64-linux-ghc-9.4.1/libHSrts-1.0.2_thr-ghc9.4.1.so
-- > x86_64-linux-ghc-9.4.1/libHSwitherable-0.4.2-1AWCu2zvFImLTaoXk8CRkT-ghc9.4.1.so
-- > x86_64-linux-ghc-9.4.1/libHSsplitmix-0.1.0.4-HUWpFUIlsWJ8kN1EGcaWa2-ghc9.4.1.so
-- > x86_64-linux-ghc-9.4.1/libHSscientific-0.3.7.0-C7AyvjqJeHGGsm9gk5kZlS-ghc9.4.1.so
-- >
-- >    ...
-- >
-- > x86_64-linux-ghc-9.4.1/libHSunix-2.7.3-ghc9.4.1.so
-- > x86_64-linux-ghc-9.4.1/libHSpretty-1.1.3.6-ghc9.4.1.so
-- > x86_64-linux-ghc-9.4.1/libHSdeepseq-1.4.8.0-ghc9.4.1.so
--
-- Notes about the value of /--ghc-options/ in command /cabal v1-build/.
--
-- - In ghc older than /8.10.6/, option /-threaded/ must be replaced with option
--   /-lHSrts_thr-ghc$(ghc --numeric-version)/ because ghc option /-flink-rts/,
--   which is passed by the module internally, has first appeared in the said
--   release,
-- - clause /ghc-options/ in the Cabal file is a better place for such a generic
--   option as /-threaded/,
-- - if the base name of the source file (/__ngx_distribution_test__.hs/) had
--   exactly matched the package name (/__ngx-distribution-test__/), then
--   options /ngx_distribution_test.hs -o ngx_distribution_test.so/ could have
--   been omitted.
--
-- Now the current working directory contains new files
-- /ngx_distribution_test.so/ and /ngx-distribution-test-0.1.0.0.tar.gz/ and a
-- new directory /x86_64-linux-ghc-9.4.1/. The tar-file contains the patched
-- shared library and the directory with dependent libraries: it is ready for
-- installation in directory /\/var\/lib\/nginx/ at the target system.

-- $setup-hs
--
-- For building custom artifacts, options of /hslibdeps/ must be accessed
-- directly. For this, commands /runhaskell Setup.hs configure \/ build/ can be
-- used instead of /cabal v1-configure \/ v1-build/. Let's change the names of
-- the directory with dependent libraries and the tar-file to /deps\// and
-- /deps.tar.gz/ respectively, and also define the /rpath/ directory without
-- using option /--prefix/.
--
-- > $ runhaskell Setup.hs configure --user --hslibdeps-options="-t/var/lib/nginx/deps -ddeps -adeps"
--
-- > $ runhaskell Setup.hs build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so -threaded"

-- $deps-cabal-v2
--
-- Nowadays, Cabal recommends building packages using /Nix-style local builds/.
-- This means that dependent packages do not get installed in places known to
-- GHC. However, they can be built inside GHC /package environments/. Let's
-- build dependencies and put them in a package environment in the current
-- working directory.
--
-- > $ cabal v2-install --lib --only-dependencies --package-env .
--
-- > $ cabal v2-install --lib ngx-export-distribution --package-env .
--
-- > $ sed -i 's/\(^package-id \)/--\1/' .ghc.environment.x86_64-linux-$(ghc --numeric-version)
--
-- This /sed/ command comments out all lines that start with word /package-id/
-- in file /.ghc.environment.x86_64-linux-9.4.1/ which has been created by the
-- former commands. This prevents the target library from linking against
-- libraries belonging to packages listed in those lines, thus making the
-- overall number and the size of dependent libraries as small as possible. If
-- this command breaks the following steps, some of the commented lines can be
-- selectively uncommented.
--
-- > $ ADD_CABAL_STORE=$(sed -n 's/^\(package-db\)\s\+/--\1=/p' .ghc.environment.x86_64-linux-$(ghc --numeric-version))
-- > $ runhaskell --ghc-arg=-package=base --ghc-arg=-package=ngx-export-distribution Setup.hs configure --package-db=clear --package-db=global $ADD_CABAL_STORE --prefix=/var/lib/nginx
--
-- Shell variable /$ADD_CABAL_STORE/ wraps all /package-db/ records found in the
-- GHC environment file into the list of options suitable for passing to the
-- /configure/ command. Normally, this list shall contain only one directory
-- /$HOME\/.cabal\/store\/ghc-$(ghc --numeric-version)\/package.db/ with all
-- packages ever built by /cabal v2-build/.
--
-- Before running the /configure/ command, we commented out all packages listed
-- in the GHC environment file. The build step requires linking the target
-- library against the direct dependencies and their dependencies in turn. With
-- [cabal-plan](https://hackage.haskell.org/package/cabal-plan), re-enabling
-- the direct dependencies in the GHC environment file can be done
-- automatically.
--
-- The following bash script collects all direct dependencies reported by
-- /cabal-plan/.
--
-- ==== File /cabal-plan-direct-deps.sh/
-- > #!/usr/bin/env bash
-- >
-- > CABAL_PLAN=$(cabal-plan info --ascii)
-- > UNIT_ID="^UnitId\s\+\""
-- > while IFS= read -r pkg
-- > do sed -n "/$UNIT_ID$pkg/s/$UNIT_ID\(.*\)\"\$/package-id \1/p" <<< "$CABAL_PLAN"
-- > done < <(sed -n '/^CompNameLib$/,/^$/s/^\s\+//p' <<< "$CABAL_PLAN")
-- > unset CABAL_PLAN UNIT_ID
--
-- After running this as
--
-- > $ . cabal-plan-direct-deps.sh >> .ghc.environment.x86_64-linux-$(ghc --numeric-version)
--
-- four lines looking similar to
--
-- > package-id aeson-2.1.0.0-9b19e87ee2a82567866c50e13806427068fd4bcc78cedb01ecad7389791f6761
-- > package-id base-4.17.0.0
-- > package-id bytestring-0.11.3.1
-- > package-id ngx-export-1.7.5-17b83e3ac354cc52614227ba662f8c23a8ddd4e08f2a1a02b0d6b51b2dd849ea
--
-- will appear at the end of file /.ghc.environment.x86_64-linux-9.4.1/. This
-- shall expose the four dependent packages at the next step.
--
-- > $ runhaskell --ghc-arg=-package=base --ghc-arg=-package=ngx-export-distribution Setup.hs build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so -threaded"
--
-- This should build library /ngx_distribution_test.so/ and link it against
-- Haskell libraries found in the global package database and the Cabal's global
-- package store.

-- $drawbacks
--
-- With all the building approaches shown above, the following list of drawbacks
-- must be taken into account.
--
-- - Utility /hslibdeps/ collects only libraries prefixed with /libHS/,
-- - clean commands such as /cabal v1-clean/ do not delete build artifacts in
--   the current working directory,
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
-- - /-dynamic/, /-shared/, /-fPIC/, /-flink-rts/ (in /ghc 8.10.6/ and newer),
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
    unless (null extraGhcOptions) $ do
        let extraSourceFile = head extraGhcOptions
        extraSourceFileExists <- doesFileExist extraSourceFile
        unless extraSourceFileExists $ ioError $ userError $
            "File " ++ extraSourceFile ++ " does not exist, " ++
            "you may want to specify input and output files in --ghc-options"
    ghcP <- fst <$> requireProgram verbosity ghcProgram (withPrograms lbi)
    let ghcR = programInvocation ghcP $
#if MIN_TOOL_VERSION_ghc(8,10,6)
            "-flink-rts" :
#endif
                ["-dynamic", "-shared", "-fPIC"] ++
                    map snd configGhcOptions ++ extraGhcOptions
    runProgramInvocation verbosity ghcR
    return lib'

-- | Patches the shared library and collects dependent Haskell libraries.
--
-- Performs the following steps.
--
-- - Collects all dependent Haskell libraries in a directory with the name equal
--   to the value of /$abi/ which normally expands to /$arch-$os-$compiler/ (or
--   with that overridden in option /--hslibdeps-options/),
-- - adds value /$prefix\/$abi/ (or that overridden in option
--   /--hslibdeps-options/) in the beginning of the list of /rpath/ in the
--   shared library,
-- - archives the shared library and the directory with the collected dependent
--   libraries in a /tar.gz/ file.
--
-- All steps are performed by utility
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
        archiveArg = "-a" : [prettyShow $ package desc]
    hslibdepsP <- fst <$> requireProgram verbosity hslibdeps (withPrograms lbi)
    let hslibdepsR = programInvocation hslibdepsP $
            lib : rpathArg ++ dirArg ++ archiveArg
    runProgramInvocation verbosity hslibdepsR

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
    simpleUserHooks { hookedPrograms = [hslibdeps]
                    , confHook = \desc flags -> do
                        let pdb = configPrograms flags
                        _ <- requireProgram verbosity hslibdeps pdb >>=
                                 requireProgram verbosity patchelf . snd
                        confHook simpleUserHooks desc flags
                    , buildHook = \desc lbi _ flags ->
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

