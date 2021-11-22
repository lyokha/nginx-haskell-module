{-# LANGUAGE TupleSections #-}

module NgxExport.Distribution (defaultMain) where

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
import System.FilePath
import Control.Exception
import Control.Arrow
import Control.Monad
import Data.Maybe

data LibNameNotSpecified = LibNameNotSpecified

instance Exception LibNameNotSpecified
instance Show LibNameNotSpecified where
    show = const "Error: the library name was not specified, \
                 \the name must be passed in ghc with option -o"

hslibdeps :: Program
hslibdeps = simpleProgram "hslibdeps"

patchelf :: Program
patchelf = simpleProgram "patchelf"

buildAndHslibdeps :: Verbosity -> PackageDescription -> LocalBuildInfo ->
    BuildFlags -> IO ()
buildAndHslibdeps verbosity desc lbi flags = do
    let configGhcOptions =
            map (("ghc", ) . pure) . concatMap snd
            . filter (\(c, o) -> c == GHC && not (null o)) $
                perCompilerFlavorToList $
                    options $ libBuildInfo $ fromJust $ library desc
        lib = fst $
            foldl (\a@(r, ready) (prog, v) ->
                if prog /= "ghc" || null v || isJust r
                    then a
                    else let v' = head v
                         in if v' == "-o"
                                then (Nothing, True)
                                else if ready
                                         then (Just v', False)
                                         else (Nothing, False)
                  ) (Nothing, False) $
                      buildProgramArgs flags ++ configGhcOptions
        env = map (second fromPathTemplate) $
            compilerTemplateEnv (compilerInfo $ compiler lbi) ++
                platformTemplateEnv (hostPlatform lbi)
        dir = maybeUnknown (lookup ArchVar env) ++
            '-' : maybeUnknown (lookup OSVar env) ++
                '-' : maybeUnknown (lookup CompilerVar env)
        dirArg = "-d" : [dir]
        maybeUnknown = fromMaybe "unknown"
        rpathArg = maybe [] (("-t" :) . pure . (</> dir) . fromPathTemplate) $
            flagToMaybe $ prefix $ configInstallDirs $ configFlags lbi
    when (isNothing lib) $ throwIO LibNameNotSpecified
    let lib' = fromJust lib
        plbi = withPrograms lbi
    ghcP <- fst <$> requireProgram verbosity ghcProgram plbi
    let ghcR = programInvocation ghcP $ ["-dynamic", "-shared", "-fPIC"] ++
            map (head . snd) configGhcOptions
    runProgramInvocation verbosity ghcR
    hslibdepsP <- fst <$> requireProgram verbosity hslibdeps plbi
    let hslibdepsR = programInvocation hslibdepsP $ lib' : rpathArg ++ dirArg
    runProgramInvocation verbosity hslibdepsR
    tarP <- fst <$> requireProgram verbosity tarProgram plbi
    let ver = pkgVersion $ package desc
        tar = addExtension (takeBaseName lib' ++ '-' : prettyShow ver) "tar.gz"
        tarR = programInvocation tarP ["czf", tar, lib', dir]
    runProgramInvocation verbosity tarR

ngxExportHooks :: UserHooks
ngxExportHooks =
    let hooks = simpleUserHooks
    in hooks { hookedPrograms = [hslibdeps]
             , confHook = \desc flags -> do
                 let pdb = configPrograms flags
                 _ <- requireProgram normal hslibdeps pdb >>=
                          requireProgram normal patchelf . snd
                 confHook simpleUserHooks desc flags
             , buildHook =  \desc lbi _ flags ->
                 buildAndHslibdeps normal desc lbi flags
             }

defaultMain :: IO ()
defaultMain = defaultMainWithHooks ngxExportHooks

