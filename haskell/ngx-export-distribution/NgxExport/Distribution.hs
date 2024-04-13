{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Distribution
-- Copyright   :  (c) Alexey Radkov 2021-2024
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

    -- *** Bootstrapping a new project
    -- $bootstrapping-project

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
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.Utils
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
-- The configuration step requires that utilities /nhm-tool/ and /patchelf/
-- were found in the paths of environment variable /$PATH/. The /nhm-tool/ is
-- packaged with this module and can be installed by running
--
-- > $ cabal install
--
-- from the root directory of the module or by running
--
-- > $ cabal install ngx-export-distribution
--
-- from any other directory, in which case it will be installed from /Hackage/.
--
-- Building is a bit cumbersome: it expects explicit option /--prefix/ at the
-- configuration step (which will be interpreted as the prefix part of the
-- /rpath/ by  /nhm-tool dist/) and explicit ghc option /-o/ at the build
-- step which is as well used by /nhm-tool dist/ as the name of the target
-- library. To avoid complexity, bootstrap the project with /nhm-tool init/.

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
-- > [2 of 2] Linking ngx_distribution_test.so
-- > ---> Collecting libraries
-- > /home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2/libHSOneTuple-0.4.1.1-GTlScb3X0Hn7Y4A5VTBpq8-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSOneTuple-0.4.1.1-GTlScb3X0Hn7Y4A5VTBpq8-ghc9.6.2.so
-- > /home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2/libHSQuickCheck-2.14.3-FxURKqK1tk15J8arEBmUtc-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSQuickCheck-2.14.3-FxURKqK1tk15J8arEBmUtc-ghc9.6.2.so
-- > /home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2/libHSStateVar-1.2.2-F3B0sJlZ41353sDhwwFm1B-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSStateVar-1.2.2-F3B0sJlZ41353sDhwwFm1B-ghc9.6.2.so
-- > /home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2/libHSaeson-2.2.0.0-KcH800TS6us8tZ6AZDtIQh-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSaeson-2.2.0.0-KcH800TS6us8tZ6AZDtIQh-ghc9.6.2.so
-- > /usr/lib64/ghc-9.6.2/lib/../lib/x86_64-linux-ghc-9.6.2/libHSarray-0.5.5.0-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSarray-0.5.5.0-ghc9.6.2.so
-- > /home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2/libHSassoc-1.1-5sFQqIOvFZQJX5kdLbEWB9-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSassoc-1.1-5sFQqIOvFZQJX5kdLbEWB9-ghc9.6.2.so
-- >
-- >    ...
-- >
-- > /home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2/libHSvector-stream-0.1.0.0-Kd51wsO6Y2s1Z8znT4c6B5-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSvector-stream-0.1.0.0-Kd51wsO6Y2s1Z8znT4c6B5-ghc9.6.2.so
-- > /home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2/libHSwitherable-0.4.2-KGoJH5wjsMLKLjx4iVJYPg-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSwitherable-0.4.2-KGoJH5wjsMLKLjx4iVJYPg-ghc9.6.2.so
-- > /lib64/libffi.so.8 -> x86_64-linux-ghc-9.6.2/libffi.so.8
-- >
-- > ---> Patching ngx_distribution_test.so
-- > /var/lib/nginx/x86_64-linux-ghc-9.6.2:/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2:/usr/lib64/ghc-9.6.2/lib/../lib/x86_64-linux-ghc-9.6.2
-- >
-- > ---> Archiving artifacts
-- > ngx_distribution_test.so
-- > x86_64-linux-ghc-9.6.2/
-- > x86_64-linux-ghc-9.6.2/libHSOneTuple-0.4.1.1-GTlScb3X0Hn7Y4A5VTBpq8-ghc9.6.2.so
-- > x86_64-linux-ghc-9.6.2/libHSQuickCheck-2.14.3-FxURKqK1tk15J8arEBmUtc-ghc9.6.2.so
-- > x86_64-linux-ghc-9.6.2/libHSStateVar-1.2.2-F3B0sJlZ41353sDhwwFm1B-ghc9.6.2.so
-- > x86_64-linux-ghc-9.6.2/libHSaeson-2.2.0.0-KcH800TS6us8tZ6AZDtIQh-ghc9.6.2.so
-- >
-- >    ...
-- >
-- > x86_64-linux-ghc-9.6.2/libHSvector-stream-0.1.0.0-Kd51wsO6Y2s1Z8znT4c6B5-ghc9.6.2.so
-- > x86_64-linux-ghc-9.6.2/libHSwitherable-0.4.2-KGoJH5wjsMLKLjx4iVJYPg-ghc9.6.2.so
-- > x86_64-linux-ghc-9.6.2/libffi.so.8
--
-- Notes about the value of /--ghc-options/ in command /cabal v1-build/.
--
-- - In ghc older than /9.0.1/, option /-threaded/ must be replaced with option
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
-- new directory /x86_64-linux-ghc-9.6.2/. The tar-file contains the patched
-- shared library and the directory with dependent libraries: it is ready for
-- installation in directory /\/var\/lib\/nginx/ at the target system.

-- $setup-hs
--
-- For building custom artifacts, options of /nhm-tool dist/ must be accessed
-- directly. For this, commands /runhaskell Setup.hs configure \/ build/ can be
-- used instead of /cabal v1-configure \/ v1-build/. Let's change the names of
-- the directory with dependent libraries and the tar-file to /deps\// and
-- /deps.tar.gz/ respectively, and also define the /rpath/ directory without
-- using option /--prefix/.
--
-- > $ runhaskell Setup.hs configure --user --nhm-tool-options="-t/var/lib/nginx/deps -ddeps -adeps"
--
--  Note that despite the name /--nhm-tool-options/, the specified options are
--  passed internally into a sub-command /nhm-tool dist/.
--
-- > $ runhaskell Setup.hs build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so -threaded"

-- $deps-cabal-v2
--
-- Nowadays, Cabal recommends building packages as /Nix-style local builds/.
-- This means that dependent packages do not get installed in places known to
-- GHC. However, they can be built inside GHC /package environments/. Let's
-- build dependencies and put them in a package environment in the current
-- working directory.
--
-- > $ cabal install --lib --only-dependencies --package-env .
--
-- This command creates both the package environment and the /build plan/. We
-- must tune the package environment by replacing existing /package-id/ records
-- with precise direct dependencies of the target library. With
-- [cabal-plan](https://hackage.haskell.org/package/cabal-plan), finding the
-- direct dependencies in the cabal build plan is easy.
--
-- > $ sed -i 's/\(^package-id \)/--\1/' .ghc.environment.x86_64-linux-$(ghc --numeric-version)
--
-- This /sed/ command comments out all lines that start with word /package-id/
-- in file /.ghc.environment.x86_64-linux-9.6.2/.
--
-- > $ nhm-tool deps ngx-distribution-test >> .ghc.environment.x86_64-linux-$(ghc --numeric-version)
--
-- Command /nhm-tool deps/ builds around the code of the /cabal-plan/ library.
-- After running this, four lines looking similar to
--
-- > package-id aeson-2.2.0.0-711db3f5b99af756f1eae54020c04616c024ecab6013b0b2140b60e4c06a6e9d
-- > package-id base-4.18.0.0
-- > package-id bytestring-0.11.4.0
-- > package-id ngx-export-1.7.7.1-7f7a3d21f396899b6466d425218188ba097f7cc49638994748bb4e4828d9e354
--
-- will appear at the end of file /.ghc.environment.x86_64-linux-9.6.2/. This
-- shall expose the four dependent packages at the next steps.
--
-- > $ ADD_CABAL_STORE=$(sed -n 's/^\(package-db\)\s\+/--\1=/p' .ghc.environment.x86_64-linux-$(ghc --numeric-version))
-- > $ ADD_DIRECT_DEPS=$(sed -n 's/^package-id\s\+\(.*\)\(-\([0-9]\+\.\)*[0-9]\+\($\|-.*\)\)/--dependency=\1=\1\2/p' .ghc.environment.x86_64-linux-$(ghc --numeric-version))
-- > $ runhaskell --ghc-arg=-package=base --ghc-arg=-package=ngx-export-distribution Setup.hs configure --package-db=clear --package-db=global $ADD_CABAL_STORE $ADD_DIRECT_DEPS --prefix=/var/lib/nginx
--
-- Shell variable /$ADD_CABAL_STORE/ wraps all /package-db/ records found in the
-- GHC environment file into the list of options suitable for passing to the
-- /configure/ command. Normally, this list shall contain only one directory
-- /$HOME\/.cabal\/store\/ghc-$(ghc --numeric-version)\/package.db/ with all
-- packages ever built by /cabal v2-build/. Variable /$ADD_DIRECT_DEPS/ does
-- similar job with /package-id/ records.
--
-- > $ runhaskell --ghc-arg=-package=base --ghc-arg=-package=ngx-export-distribution Setup.hs build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so -threaded"
--
-- This should build library /ngx_distribution_test.so/ and link it against
-- Haskell libraries found in the global package database and the Cabal package
-- store.

-- $bootstrapping-project
--
-- All the steps listed in the previous section can be automated. This is
-- exactly what /nhm-tool init/ does. Running
--
-- > $ nhm-tool init project-name
--
-- produces files /cabal.project/, /Setup.hs/, /project-name.cabal/, /Makefile/,
-- /project_name.hs/, and /hie.yaml/. If any of the files exist, add option /-f/
-- to override them. File /project_name.hs/ is not overridable.
--
-- Note that the Haskell source file is /project_name.hs/ where /project_name/
-- is /project-name/ with all dashes replaced by underscores. If the source code
-- will depend on packages other than /base/ and /ngx-export/, add them into
-- /project-name.cabal/ manually.
--
-- By default, the target library will be linked against the threaded Haskell
-- RTS library. To link against the base RTS library, add option /-no-threaded/.
--
-- The target library will be installed in directory /\/var\/lib\/nginx/. Use
-- option /-p prefix/ to override the install directory.
--
-- Build and install the target library.
--
-- > $ make
-- > $ sudo make install
--
-- With ghc older than /9.0.1/, build with
--
-- > $ make LINKRTS=-lHSrts_thr-ghc$(ghc --numeric-version)
--
-- To delete intermediate build files, run
--
-- > $ make clean
--
-- or, to additionally delete the GHC environment file and the built artifact,
--
-- > $ make clean-all
--
-- To just create the GHC package environment, run
--
-- > $ make env
--
-- This is enough to start editing the Haskell source file in an editor which
-- has support for the /Haskell Language Server/.

-- $drawbacks
--
-- With all the building approaches shown above, the following list of drawbacks
-- must be taken into account.
--
-- - Utility /nhm-tool/ collects only libraries prefixed with /libHS/ or
--   /libffi.so/,
-- - clean commands such as /cabal v1-clean/ do not delete build artifacts in
--   the current working directory,
-- - behavior of Cabal commands other than /configure/, /build/ and /clean/ is
--   not well defined.

nhmTool :: Program
nhmTool = simpleProgram "nhm-tool"

patchelf :: Program
patchelf = simpleProgram "patchelf"

-- | Builds a shared library.
--
-- Runs /ghc/ compiler with the following arguments.
--
-- - /-dynamic/, /-shared/, /-fPIC/, /-flink-rts/ (in /ghc 9.0.1/ and newer),
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
                       nameSo = name <.> "so"
                   in (nameSo, [name <.> "hs", "-o", nameSo])
                  ) (, []) lib
    case extraGhcOptions of
        extraSourceFile : _ -> do
            extraSourceFileExists <- doesFileExist extraSourceFile
            unless extraSourceFileExists $ die' verbosity $
                "File " ++ extraSourceFile ++ " does not exist, you may want \
                    \to specify input and output files in --ghc-options"
        _ -> return ()
    ghcP <- fst <$> requireProgram verbosity ghcProgram (withPrograms lbi)
    let libGhcOptions = ["-dynamic", "-shared", "-fPIC"]
        libGhcOptions' = if programVersion ghcP >= Just (mkVersion [9, 0, 1])
                             then "-flink-rts" : libGhcOptions
                             else libGhcOptions
        ghcR = programInvocation ghcP $
            libGhcOptions' ++ map snd configGhcOptions ++ extraGhcOptions
    runProgramInvocation verbosity ghcR
    return lib'

-- | Patches the shared library and collects dependent Haskell libraries.
--
-- Performs the following steps.
--
-- - Collects all dependent Haskell libraries in a directory with the name equal
--   to the value of /$abi/ which normally expands to /$arch-$os-$compiler/ (or
--   with that overridden in option /--nhm-tool-options/),
-- - adds value /$prefix\/$abi/ (or that overridden in option
--   /--nhm-tool-options/) in the beginning of the list of /rpath/ in the
--   shared library,
-- - archives the shared library and the directory with the collected dependent
--   libraries in a /tar.gz/ file.
--
-- All steps are performed by utility /nhm-tool/. It collects all libraries
-- with prefixes /libHS/ or /libffi.so/ from the list returned by command /ldd/
-- applied to the shared library.
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
    nhmToolP <- fst <$> requireProgram verbosity nhmTool (withPrograms lbi)
    let nhmToolR = programInvocation nhmToolP $
            "dist" : lib : "-v" : rpathArg ++ dirArg ++ archiveArg
    runProgramInvocation verbosity nhmToolR

-- | Build hooks.
--
-- Based on 'simpleUserHooks'. Overrides
--
-- - 'confHook' by configuring programs /nhm-tool/ and /patchelf/ and then
--   running the original /confHook/ from /simpleUserHooks/,
-- - 'buildHook' by running in sequence 'buildSharedLib' and
--   'patchAndCollectDependentLibs'.
--
-- Other hooks from /simpleUserHooks/ get derived as is. Running them is
-- neither tested nor recommended.
ngxExportHooks :: UserHooks
ngxExportHooks =
    simpleUserHooks { hookedPrograms = [nhmTool]
                    , confHook = \desc flags -> do
                        let verbosity = toVerbosity $ configVerbosity flags
                            pdb = configPrograms flags
                        _ <- requireProgram verbosity nhmTool pdb >>=
                                 requireProgram verbosity patchelf . snd
                        confHook simpleUserHooks desc flags
                    , buildHook = \desc lbi _ flags -> do
                        let verbosity = toVerbosity $ buildVerbosity flags
                        buildSharedLib verbosity desc lbi flags >>= \lib ->
                            patchAndCollectDependentLibs verbosity lib desc lbi
                    }
    where toVerbosity = fromFlagOrDefault normal

-- | A simple implementation of /main/ for a Cabal setup script.
--
-- Implemented as
--
-- @
-- defaultMain = 'defaultMainWithHooks' 'ngxExportHooks'
-- @
defaultMain :: IO ()
defaultMain = defaultMainWithHooks ngxExportHooks

