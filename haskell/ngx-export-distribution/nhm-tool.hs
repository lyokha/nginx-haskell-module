{-# LANGUAGE RecordWildCards, LambdaCase, MultiWayIf, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Parsec hiding (uncons)
import Distribution.Simple.Program
import Distribution.Verbosity
import Cabal.Plan
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Char
import Data.List
import System.Console.ANSI
import System.Environment
import System.Directory
import System.FilePath
import System.Exit
import System.IO
import Data.Version (showVersion)
import Paths_ngx_export_distribution (version)

data DistData = DistData { distDataDir :: String
                         , distDataArchive :: String
                         , distDataTargetDir :: String
                         , distDataPatchOnly :: Bool
                         , distDataTargetLib :: String
                         , distDataWait :: Maybe ArgWait
                         , distDataOwnVerbosity :: Verbosity
                         , distDataOtherVerbosity :: Verbosity
                         , distDataHelp :: Bool
                         }

defaultDistDataDir :: String
defaultDistDataDir = ".hslibs"

data DepsData = DepsData { depsDataBuilddir :: String
                         , depsDataProject :: String
                         , depsDataWait :: Maybe ArgWait
                         , depsDataHelp :: Bool
                         }

data InitData = InitData { initDataPrefix :: String
                         , initDataNoThreaded :: Bool
                         , initDataForce :: Bool
                         , initDataToStdout :: Bool
                         , initDataProject :: String
                         , initDataWait :: Maybe ArgWait
                         , initDataHelp :: Bool
                         }

defaultInitDataPrefix :: String
defaultInitDataPrefix = "/var/lib/nginx"

data ArgWait = ArgWaitA | ArgWaitD | ArgWaitT | ArgWaitP

data LddRec = LibHS String (Maybe FilePath)
            | LibFFI String (Maybe FilePath)
            | LibOther String (Maybe FilePath)
            deriving Show

data HelpSection = HelpDist | HelpDeps | HelpInit deriving Eq

progVersion :: String
progVersion = "nhm-tool " ++ showVersion version

usage :: Maybe HelpSection -> Bool -> IO ()
usage section success = do
    when (isNothing section) $
        putStrLn $ progVersion ++
            ": help building custom Haskell handlers for Nginx,\n\
            \this is a tool from \
            \https://github.com/lyokha/nginx-haskell-module\n"
    putStrLn "Usage:"
    when (isNothing section || section == Just HelpDist) $
        T.putStrLn $ T.concat ["\n\
        \  * nhm-tool dist [-d dir -a ar | -p] [-t target_dir] [-v | -vv] \
        \lib\n\n\
        \    collect Haskell libraries on which 'lib' depends,\n\
        \    patch 'lib' to enable loading dependent libraries from \
        \'target_dir'\n\n\
        \    'dir' is a directory where dependent libraries will be collected\n\
        \      (default is ", T.pack defaultDistDataDir, ")\n\
        \    'target_dir' is a directory where dependent libraries will be\n\
        \      installed (no default, 'lib' will not be patched if omitted)\n\
        \    'ar' is the base name of the archive to contain 'lib' and\n\
        \      'dir' (no default, the archive will not be created if omitted)\n\
        \    if '-p' (patch-only) is specified then dependent libraries will\n\
        \      neither be collected nor archived\n\
        \    use options '-v' and '-vv' to increase verbosity level\n\
        \    special value '-' for 'dir', 'target_dir', and 'ar' resets them\n\
        \      (with -d- being equivalent to -p)"
        ]
    when (isNothing section || section == Just HelpDeps) $
        T.putStrLn "\n\
        \  * nhm-tool deps project-name [-d dir]\n\n\
        \    print all direct dependencies of 'project-name',\n\
        \    the output is compatible with the format of GHC environment \
        \files\n\n\
        \    'dir' is the cabal build directory where the build plan is \
        \located\n\
        \      (default is dist-newstyle)"
    when (isNothing section || section == Just HelpInit) $
        T.putStrLn $ T.concat ["\n\
        \  * nhm-tool init [-p dir] [-no-threaded] [-f | -to-stdout] \
        \project-name\n\n\
        \    bootstrap environment to build custom Haskell handlers\n\n\
        \    '-p' prefix for install dir, default is ",
        T.pack defaultInitDataPrefix, "\n\
        \    '-no-threaded' use base RTS library\n\
        \    '-f' force re-writing of existing files (except the source file)\n\
        \    '-to-stdout' do not write files but dump what will be written"
        ]
    when (isNothing section) $
        T.putStrLn "\n\
        \  * nhm-tool [-h | -help | --help | -v | -version | --version]\n\n\
        \    show this help message or version and exit,\n\
        \    help options are applicable in sub-commands as well"
    if success
        then exitSuccess
        else exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        "dist" : args' -> do
            let distData = foldl parseDistArg (Just defaultArgs) args'
                defaultArgs = DistData defaultDistDataDir "" "" False ""
                    Nothing normal normal False
            case distData of
                Nothing -> usage (Just HelpDist) False
                Just (normalizeDistData -> distData'@DistData {..}) ->
                    if | distDataHelp ->
                             usage (Just HelpDist) $ length args' == 1
                       | isJust distDataWait
                         || null distDataTargetLib ->
                             usage (Just HelpDist) False
                       | otherwise -> cmdDist distData'
        "deps" : args' -> do
            let depsData = foldl parseDepsArg (Just defaultArgs) args'
                defaultArgs = DepsData "" "" Nothing False
            case depsData of
                Nothing -> usage (Just HelpDeps) False
                Just depsData'@DepsData {..} ->
                    if | depsDataHelp ->
                             usage (Just HelpDeps) $ length args' == 1
                       | isJust depsDataWait
                         || null depsDataProject ->
                             usage (Just HelpDeps) False
                       | otherwise -> cmdDeps depsData'
        "init" : args' -> do
            let initData = foldl parseInitArg (Just defaultArgs) args'
                defaultArgs = InitData defaultInitDataPrefix False False False
                    "" Nothing False
            case initData of
                Nothing -> usage (Just HelpInit) False
                Just initData'@InitData {..} ->
                    if | initDataHelp ->
                             usage (Just HelpInit) $ length args' == 1
                       | isJust initDataWait
                         || null initDataProject
                         || initDataForce && initDataToStdout ->
                             usage (Just HelpInit) False
                       | otherwise -> cmdInit initData'
        [arg] | arg `elem` ["-h", "-help", "--help"] ->
                  usage Nothing True
              | arg `elem` ["-v", "-version", "--version"] ->
                  putStrLn progVersion >> exitSuccess
        args' -> usage Nothing $ null args'
    where normalizeDistData dist@DistData {..} =
              dist { distDataTargetDir =
                         if distDataTargetDir == "-"
                             then ""
                             else distDataTargetDir
                   , distDataArchive =
                         if distDataArchive == "-"
                             then ""
                             else distDataArchive
                   , distDataPatchOnly =
                         distDataDir == "-" || distDataPatchOnly
                   }

parseDistArg :: Maybe DistData -> String -> Maybe DistData
parseDistArg Nothing _ = Nothing
parseDistArg (Just dist@DistData {..}) arg =
    case distDataWait of
        Nothing ->
            let optLong = length arg > 2
                optValue = drop 2 arg
            in if | "-d" `isPrefixOf` arg ->
                        if optLong
                            then Just dist' { distDataDir = optValue }
                            else Just dist { distDataWait = Just ArgWaitD }
                  | "-t" `isPrefixOf` arg ->
                        if optLong
                            then Just dist' { distDataTargetDir = optValue }
                            else Just dist { distDataWait = Just ArgWaitT }
                  | "-a" `isPrefixOf` arg ->
                        if optLong
                            then Just dist' { distDataArchive = optValue }
                            else Just dist { distDataWait = Just ArgWaitA }
                  | "-p" == arg ->
                        Just dist' { distDataPatchOnly = True }
                  | "-v" == arg ->
                        Just dist' { distDataOwnVerbosity = verbose }
                  | "-vv" == arg ->
                        Just dist' { distDataOwnVerbosity = verbose
                                   , distDataOtherVerbosity = verbose
                                   }
                  | "-h" == arg || "-help" == arg || "--help" == arg ->
                        Just dist' { distDataHelp = True }
                  | "-" `isPrefixOf` arg -> Nothing
                  | null distDataTargetLib ->
                        Just dist' { distDataTargetLib = arg }
                  | otherwise -> Nothing
        Just ArgWaitD -> Just dist' { distDataDir = arg }
        Just ArgWaitT -> Just dist' { distDataTargetDir = arg }
        Just ArgWaitA -> Just dist' { distDataArchive = arg }
        Just _ -> undefined
        where dist' = dist { distDataWait = Nothing }

parseDepsArg :: Maybe DepsData -> String -> Maybe DepsData
parseDepsArg Nothing _ = Nothing
parseDepsArg (Just deps@DepsData {..}) arg =
    case depsDataWait of
        Nothing ->
            let optLong = length arg > 2
                optValue = drop 2 arg
            in if | "-d" `isPrefixOf` arg ->
                        if optLong
                            then Just deps' { depsDataBuilddir = optValue }
                            else Just deps' { depsDataWait = Just ArgWaitD }
                  | "-h" == arg || "-help" == arg || "--help" == arg ->
                        Just deps' { depsDataHelp = True }
                  | "-" `isPrefixOf` arg -> Nothing
                  | null depsDataProject ->
                        Just deps' { depsDataProject = arg }
                  | otherwise -> Nothing
        Just ArgWaitD -> Just deps' { depsDataBuilddir = arg }
        Just _ -> undefined
        where deps' = deps { depsDataWait = Nothing }

parseInitArg :: Maybe InitData -> String -> Maybe InitData
parseInitArg Nothing _ = Nothing
parseInitArg (Just init'@InitData {..}) arg =
    case initDataWait of
        Nothing ->
            let optLong = length arg > 2
                optValue = drop 2 arg
            in if | "-p" `isPrefixOf` arg ->
                        if optLong
                            then Just init'' { initDataPrefix = optValue }
                            else Just init' { initDataWait = Just ArgWaitP }
                  | "-no-threaded" == arg ->
                        Just init'' { initDataNoThreaded = True }
                  | "-f" == arg ->
                        Just init'' { initDataForce = True }
                  | "-to-stdout" == arg ->
                        Just init'' { initDataToStdout = True }
                  | "-h" == arg || "-help" == arg || "--help" == arg ->
                        Just init'' { initDataHelp = True }
                  | "-" `isPrefixOf` arg -> Nothing
                  | null initDataProject ->
                        Just init'' { initDataProject = arg }
                  | otherwise -> Nothing
        Just ArgWaitP -> Just init'' { initDataPrefix = arg }
        Just _ -> undefined
        where init'' = init' { initDataWait = Nothing }

cmdDist :: DistData -> IO ()
cmdDist DistData {..} = do
    let pdb = emptyProgramDb
        patchelf = simpleProgram "patchelf"
        ldd = simpleProgram "ldd"
    (patchelf', pdb') <- requireProgram distDataOtherVerbosity patchelf pdb
    if distDataPatchOnly
        then patchTargetLib patchelf'
        else do
            (ldd', pdb'') <- requireProgram distDataOtherVerbosity ldd pdb'
            putStrLn' "---> Collecting libraries"
            lddOut <- getProgramOutput distDataOtherVerbosity
                ldd' [distDataTargetLib]
            case parseLddOutput lddOut of
                Left err -> do
                    hPutStrLn stderr $ show err ++ " in\n" ++ lddOut
                    exitFailure
                Right recs -> do
                    (tar', _) <- requireProgram distDataOtherVerbosity
                        tarProgram pdb''
                    collectLibs recs lddOut
                    unless (null distDataTargetDir) $ putStrLn' ""
                    patchTargetLib patchelf'
                    putStrLn' ""
                    archiveLibs tar'
    where patchTargetLib patchelf' =
              unless (null distDataTargetDir) $ do
                  putStrLn' $ "---> Patching " ++ distDataTargetLib
                  patchelfOut <- getProgramOutput distDataOtherVerbosity
                      patchelf' ["--print-rpath", distDataTargetLib]
                  case parsePatchelfRpathOutput patchelfOut of
                      Left err -> do
                          hPutStrLn stderr $ show err ++ " in\n" ++ patchelfOut
                          exitFailure
                      Right paths -> do
                          unless (distDataTargetDir `elem` paths) $
                              runProgram distDataOtherVerbosity patchelf'
                                  ["--set-rpath"
                                  ,distDataTargetDir ++ ':' : patchelfOut
                                  ,distDataTargetLib
                                  ]
                          patchelfOut' <- getProgramOutput
                              distDataOtherVerbosity
                                  patchelf' ["--print-rpath"
                                            ,distDataTargetLib
                                            ]
                          putStrLnTrim patchelfOut'
          collectLibs recs lddOut = do
              let recsLibHS = M.fromList $
                      mapMaybe (\case
                                    LibHS name path -> Just (name, path)
                                    LibFFI name path -> Just (name, path)
                                    _ -> Nothing
                               ) recs
              if M.null recsLibHS
                  then do
                      hPutStrLn stderr $
                          "No Haskell libraries were collected in\n" ++ lddOut
                      exitFailure
                  else do
                      let (M.mapMaybe id -> recsLibHS', recsLibHSNotFound) =
                              M.partition isJust recsLibHS
                      if M.null recsLibHSNotFound
                          then do
                              createDirectoryIfMissing False distDataDir
                              forM_ (M.elems recsLibHS') $ \path -> do
                                  let dst = distDataDir </> takeFileName path
                                  putStrLn' $ path ++ " -> " ++ dst
                                  copyFile path dst
                          else do
                              hPutStrLn stderr $
                                 "Haskell libraries " ++
                                     show (M.keys recsLibHSNotFound) ++
                                         " were not found in\n" ++ lddOut
                              exitFailure
          archiveLibs tar' =
              unless (null distDataArchive) $ do
                  putStrLn' "---> Archiving artifacts"
                  tarOut <- getProgramOutput distDataOtherVerbosity tar'
                      ["czvf"
                      ,distDataArchive <.> ".tar.gz"
                      ,distDataTargetLib
                      ,distDataDir
                      ]
                  putStrLnTrim tarOut
          putStrLn' = when (distDataOwnVerbosity == verbose) . putStrLn
          putStrLnTrim = putStrLn' . trimEnd '\n'
          trimEnd end = fst . foldr (\v a@(vs, skipped) ->
                                         if skipped || v /= end
                                             then (v : vs, True)
                                             else a
                                    ) ("", False)

parsePatchelfRpathOutput :: String -> Either ParseError [String]
parsePatchelfRpathOutput =
    parse (many (satisfy (/= colon)) `sepBy` char colon) "patchelf_rpath"
    where colon = ':'

parseLddOutput :: String -> Either ParseError [LddRec]
parseLddOutput = flip parse "ldd" $ many $
    spaces *>
    (try (toLddRec <$>
              manyTill anyChar' sep <*>
                  ((Just .) . (:) <$> char '/' <*> right
                   <|> Nothing <$ string "not found"
                  )
         )
     <|> try ((`LibOther` Nothing) <$> right)
     <|> (`LibOther` Nothing) <$> manyTill anyChar' (sep *> addr *> newline)
    )
    where toLddRec lib | "libHS" `isPrefixOf` lib = LibHS lib
                       | "libffi.so" `isPrefixOf` lib = LibFFI lib
                       | otherwise = LibOther lib
          right = manyTill anyChar' $ spaces1 *> addr *> newline
          addr = string "(0x" *> many1 hexDigit *> char ')'
          anyChar' = satisfy (/= '\n')
          sep = spaces1 *> string "=>" *> spaces1
          spaces1 = skipMany1 space

cmdDeps :: DepsData -> IO ()
cmdDeps DepsData {..} = do
    let buildDir = if null depsDataBuilddir
                       then ProjectRelativeToDir "."
                       else InBuildDir depsDataBuilddir
    units <- pjUnits <$> findAndDecodePlanJson buildDir
    let comps = [ uComps
                | Unit {..} <- M.elems units
                , uType == UnitTypeLocal
                , let uPkgName = (\(PkgId (PkgName name) _) -> name) uPId
                , uPkgName == T.pack depsDataProject
                ]
    when (null comps) $ do
        hPutStrLn stderr $ "Failed to find plan for " ++ depsDataProject
        exitFailure
    let deps = foldl (\a curComps ->
                          let libComps = M.filterWithKey
                                  (const . (== CompNameLib)) curComps
                          in M.foldr S.union a $ M.map ciLibDeps libComps
                     ) S.empty comps
    forM_ (S.toList deps) $ \(UnitId unit) ->
        putStrLn $ "package-id " ++ T.unpack unit

cmdInit :: InitData -> IO ()
cmdInit init'@InitData {..} = do
    let files = [("cabal.project", cabalProject init', True)
                ,("Setup.hs", setupHs init', True)
                ,(initDataProject ++ ".cabal", projectCabal init', True)
                ,("Makefile", makefile init', True)
                ,(replace '-' '_' initDataProject ++ ".hs"
                 ,projectHs init'
                 ,False
                 )
                ]
    forM_ files $
        if initDataToStdout
            then \(name, file, _) ->
                printHeader (name ++ "\n") >> T.putStrLn file
            else \(name, file, overridable) -> do
                exists <- doesFileExist name
                if exists
                    then if initDataForce && overridable
                             then T.writeFile name file
                             else hPutStrLn stderr $
                                 if overridable
                                     then useForceMsg name
                                     else existsMsg name
                    else T.writeFile name file
    where replace from to = foldr (\v -> ((if v == from then to else v) :)) ""
          printHeader header = do
              isTerm <- hIsTerminalDevice stdout
              if isTerm
                  then do
                      setSGR [SetColor Foreground Dull Blue
                             ,SetUnderlining SingleUnderline
                             ]
                      putStrLn header
                      setSGR [Reset]
                  else putStrLn $ " ~~~ " ++ header
          existsMsg name = "File " ++ name ++ " exists"
          useForceMsg name = existsMsg name ++ ", use option -f to override it"

cabalProject :: InitData -> Text
cabalProject InitData {..} = T.concat
    ["packages: ", T.pack initDataProject, ".cabal\n"]

setupHs :: InitData -> Text
setupHs = const
    "import NgxExport.Distribution\n\
    \main = defaultMain\n"

projectCabal :: InitData -> Text
projectCabal InitData {..} = T.concat
    ["name:                       ", T.pack initDataProject, "\n\
     \version:                    0.1.0.0\n\
     \build-type:                 Custom\n\
     \cabal-version:              2.0\n\
     \\n\
     \custom-setup\n\
     \  setup-depends:            base >= 4.8 && < 5\n\
     \                          , ngx-export-distribution\n\
     \\n\
     \library\n\
     \  default-language:         Haskell2010\n\
     \  build-tool-depends:       ngx-export-distribution:nhm-tool\n\
     \  build-depends:            base >= 4.8 && < 5\n\
     \                          , ngx-export\n\
     \\n\
     \  ghc-options:             -Wall -O2\n"
    ,if initDataNoThreaded
         then ""
         else
     "\n\
     \  if impl(ghc >= 9.0.1)\n\
     \    ghc-options:           -threaded\n"
    ]

makefile :: InitData -> Text
makefile InitData {..} = T.concat
    ["NAME := ", T.replace "-" "_" $ T.pack initDataProject, "\n\
     \PKGNAME := $(subst _,-,$(NAME))\n\
     \PKGVER := 0.1.0.0\n\
     \\n\
     \PREFIX := ", T.pack initDataPrefix, "\n\
     \MACHINE := $(shell uname -m)\n\
     \KERNEL := $(shell uname -s | tr A-Z a-z)\n\
     \\n\
     \PKGDISTR := ngx-export-distribution\n\
     \NHMTOOL := nhm-tool\n\
     \\n\
     \SRC := $(NAME).hs\n\
     \LIB := $(NAME).so\n\
     \STUB := $(NAME)_stub.h\n\
     \DISTR := $(PKGNAME)-$(PKGVER).tar.gz\n\
     \\n\
     \GHCVER := $(shell ghc --numeric-version)\n\
     \GHCENV := .ghc.environment.$(MACHINE)-$(KERNEL)-$(GHCVER)\n\
     \DEPLIBS := $(MACHINE)-$(KERNEL)-ghc-$(GHCVER)\n\
     \BUILDDIR := dist-nhm\n\
     \\n\
     \all: $(DISTR)\n\
     \\n\
     \$(DISTR): $(SRC)\n\
     \\tcabal install --builddir=\"$(BUILDDIR)\" --lib --only-dependencies \\\n\
     \\t  --package-env .\n\
     \\tsed -i 's/\\(^package-id \\)/--\\1/' $(GHCENV)\n\
     \\tif ! command -v $(NHMTOOL) >/dev/null; then \\\n\
     \\t  PATH=$$(dirname $$(cabal list-bin $(PKGDISTR))):$$PATH; \\\n\
     \\tfi; \\\n\
     \\t$(NHMTOOL) deps $(PKGNAME) -d \"$(BUILDDIR)\" >> $(GHCENV); \\\n\
     \\trunhaskell --ghc-arg=-package=base \\\n\
     \\t  --ghc-arg=-package=$(PKGDISTR) Setup.hs configure \\\n\
     \\t  --builddir=\"$(BUILDDIR)\" \\\n\
     \\t  --package-db=clear --package-db=global \\\n\
     \\t  $$(sed -n 's/^\\(package-db\\)\\s\\+/--\\1=/p' $(GHCENV)) \\\n\
     \\t  $$(sed -n 's/^package-id\\s\\+\\(.*\\)'` \\\n\
     \\t    `'\\(-\\([0-9]\\+\\.\\)*[0-9]\\+\\($$\\|-.*\\)\\)/'` \\\n\
     \\t    `'--dependency=\\1=\\1\\2/p' \\\n\
     \\t    $(GHCENV)) \\\n\
     \\t  --prefix=$(PREFIX); \\\n\
     \\trunhaskell --ghc-arg=-package=base \\\n\
     \\t  --ghc-arg=-package=$(PKGDISTR) Setup.hs build \\\n\
     \\t  --builddir=\"$(BUILDDIR)\" \\\n\
     \\t  --ghc-options=\"$(SRC) -o $(LIB) $(LINKRTS)\"\n\
     \\n\
     \install: $(DISTR)\n\
     \\tinstall -d $(PREFIX)\n\
     \\ttar xf $(DISTR) -C $(PREFIX) --no-same-owner\n\
     \\n\
     \.PHONY: clean\n\
     \\n\
     \clean:\n\
     \\trm -rf $(BUILDDIR) $(DEPLIBS)\n\
     \\trm -f $(GHCENV) $(STUB) $(NAME).hi $(NAME).o\n\
     \\trm -f $(LIB)\n\
     \\n\
     \clean-all: clean\n\
     \\trm -f $(DISTR)\n"
    ]

projectHs :: InitData -> Text
projectHs InitData {..} = T.concat
    ["{-# LANGUAGE TemplateHaskell #-}\n\n\
     \module "
    ,T.pack $ foldr (\v -> if v == '-' || v == '_'
                               then maybe [] (uncurry $ (:) . toUpper) . uncons
                               else (v :)
                    ) "" $ '_' : initDataProject
    ," where\n\n"
    ]

