{-# LANGUAGE CPP, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Distribution
-- Copyright   :  (c) Alexey Radkov 2021-2022
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
--                            -package=base
--                            -package=ngx-export
--                            -package=bytestring
--                            -package=aeson
-- @
--
-- All packages listed in /build-depends/ get also wrapped inside options
-- /-package/ in /ghc-options/: this is important when building them with
-- /cabal v2-build/ and then using inside GHC /package environments/.
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
-- ==== Building with cabal v1-commands
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
-- > [1 of 1] Compiling Main             ( dist/setup/setup.hs, dist/setup/Main.o )
-- > Linking ./dist/setup/setup ...
-- > Configuring ngx-distribution-test-0.1.0.0...
--
-- > $ cabal v1-build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so -threaded"
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
-- >
-- > ---> Archiving artifacts
-- > ngx_distribution_test.so
-- > x86_64-linux-ghc-8.10.5/
-- > x86_64-linux-ghc-8.10.5/libHSasync-2.2.4-ENjuIeC23kaKyMVDRYThP3-ghc8.10.5.so
-- > x86_64-linux-ghc-8.10.5/libHSsplitmix-0.1.0.4-HVTAcdRNxuE9ndjT7sldq9-ghc8.10.5.so
-- > x86_64-linux-ghc-8.10.5/libHSth-abstraction-0.4.3.0-5HX1AugCZKLKm3ZYKErCAM-ghc8.10.5.so
-- > x86_64-linux-ghc-8.10.5/libHSrts_thr-ghc8.10.5.so
-- >
-- >    ...
-- >
-- > x86_64-linux-ghc-8.10.5/libHSbifunctors-5.5.11-2fVsEc2ZlypEgv2Pi5nRwa-ghc8.10.5.so
-- > x86_64-linux-ghc-8.10.5/libHSstrict-0.4.0.1-Bs4t4Fhsgeo8grcWS7WJTy-ghc8.10.5.so
-- > x86_64-linux-ghc-8.10.5/libHSdlist-1.0-GVPedlNIGcrCE31hGMMV1G-ghc8.10.5.so
--
-- Note that in ghc older than /8.10.6/, option /-threaded/ must be replaced
-- with option /-lHSrts_thr-ghc$(ghc --numeric-version)/ because ghc option
-- /-flink-rts/ which is passed by the module internally has first appeared in
-- the said release. Note also that clause /ghc-options/ in the Cabal file is a
-- better place for such a generic option as /-threaded/.
--
-- Now the current working directory contains new files
-- /ngx_distribution_test.so/ and /ngx-distribution-test-0.1.0.0.tar.gz/ and a
-- new directory /x86_64-linux-ghc-8.10.5/. The tar-file contains the patched
-- shared library and the directory with dependent libraries: it is ready for
-- installation in directory /\/var\/lib\/nginx/ at the target system.
--
-- ==== Building with Setup.hs commands
--
-- For building custom artifacts, options of /hslibdeps/ must be accessed
-- directly. For this, commands /runhaskell Setup.hs configure \/ build/ can be
-- used instead of /cabal v1-configure \/ v1-build/. Let's change the names of
-- the directory with dependent libraries and the tar-file to /deps\// and
-- /deps.tar.gz/ respectively, and also define the /rpath/ directory without
-- using option /--prefix/.
--
-- > $ runhaskell Setup.hs configure --user
--
-- > $ runhaskell Setup.hs build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so -threaded" --hslibdeps-options="-t/var/lib/nginx/deps -ddeps -adeps"
--
-- ==== Building dependencies with cabal v2-build
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
-- in file /.ghc.environment.x86_64-linux-8.10.5/ which has been created by the
-- former commands. This prevents the target library from linking against
-- libraries listed in those lines thus making the overall number and the size
-- of dependent libraries as small as possible. If this command breaks the
-- following steps, some of the commented lines can be selectively uncommented.
--
-- > $ runhaskell --ghc-arg=-package=base --ghc-arg=-package=ngx-export-distribution Setup.hs configure --package-db=clear --package-db=global --package-db="$HOME/.cabal/store/ghc-$(ghc --numeric-version)/package.db" --prefix=/var/lib/nginx
--
-- Directory /$HOME\/.cabal\/store\/ghc-$(ghc --numeric-version)\/package.db/
-- contains a GHC /package db/ with all packages built by /cabal v2-build/, it
-- gets also listed in file /.ghc.environment.x86_64-linux-8.10.5/.
--
-- > $ runhaskell --ghc-arg=-package=base --ghc-arg=-package=ngx-export-distribution Setup.hs build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so -threaded"
--
-- This should build library /ngx_distribution_test.so/ and link it against
-- Haskell libraries found in the global package db and
-- /$HOME\/.cabal\/store\/ghc-$(ghc --numeric-version)\/package.db/.
--
-- ==== Collecting direct dependencies with cabal-plan
--
-- We listed build dependencies in both /build-depends/ and /ghc-options/
-- clauses in the Cabal file to let ghc find dependencies built with
-- /cabal v2-build/ at the /configure/ step. This approach is tedious and
-- error-prone. Fortunately, there is package
-- [cabal-plan](https://hackage.haskell.org/package/cabal-plan) which is aimed
-- to figure out dependencies of built packages. Particularly, with /cabal-plan/
-- we can remove those /--package=.../ lines from the /ghc-options/ clause in
-- the Cabal file and, instead, collect them programmatically in a shell
-- variable that will be put inside the /configure/ command.
--
-- > $ DIRECT_DEPS=$(cabal-plan info --ascii | sed -n -e '0,/^CompNameLib$/d' -e '/^$/,$d' -e 's/^\s\+/--package=/p')
-- > $ runhaskell --ghc-arg=-package=base --ghc-arg=-package=ngx-export-distribution Setup.hs configure --package-db=clear --package-db=global --package-db="$HOME/.cabal/store/ghc-$(ghc --numeric-version)/package.db" $DIRECT_DEPS --prefix=/var/lib/nginx
--
-- ==== Drawbacks
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

