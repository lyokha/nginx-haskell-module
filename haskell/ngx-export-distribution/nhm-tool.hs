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
                         , distDataOwnVerbosity :: Verbosity
                         , distDataOtherVerbosity :: Verbosity
                         , distDataWaitArg :: Maybe String
                         , distDataHelp :: Bool
                         }

defaultDistDataDir :: String
defaultDistDataDir = ".hslibs"

data PlanData = PlanData { planDataBuilddir :: String
                         , planDataQuery :: Maybe PlanDataQuery
                         , planDataWaitArg :: Maybe String
                         , planDataWaitProjectName :: Bool
                         , planDataHelp :: Bool
                         }

data PlanDataQuery = PlanDataQueryDeps String
                   | PlanDataQueryAbi

data InitData = InitData { initDataPrefix :: String
                         , initDataNoThreaded :: Bool
                         , initDataForce :: Bool
                         , initDataToStdout :: Bool
                         , initDataProject :: String
                         , initDataGhcOptions :: [String]
                         , initDataWaitArg :: Maybe String
                         , initDataHelp :: Bool
                         }

defaultInitDataPrefix :: String
defaultInitDataPrefix = "/var/lib/nginx"

data LddRec = LibHS String (Maybe FilePath)
            | LibFFI String (Maybe FilePath)
            | LibOther String (Maybe FilePath)
            deriving Show

data HelpSection = HelpDist | HelpPlan | HelpDeps | HelpInit deriving Eq

replaceChar :: Char -> Char -> String -> String
replaceChar from to = map $ \v -> if v == from then to else v

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
    when (isNothing section || section `elem` map Just [HelpPlan, HelpDeps]) $
        T.putStrLn "\n\
        \  * nhm-tool plan [-d dir] [deps project-name | abi]\n\n\
        \    print build plan or its derivatives:\n\
        \    'deps' print all direct dependencies of 'project-name',\n\
        \      the output is compatible with the format of GHC environment \
        \files\n\
        \    'abi' print 'Arch-Os-CompilerId' bundle in GHC style\n\n\
        \    'dir' is the cabal build directory where the build plan is \
        \located\n\
        \      (default is dist-newstyle)"
    when (isNothing section || section == Just HelpDeps) $
        T.putStrLn "\n\
        \  * nhm-tool deps [-d dir] project-name\n\n\
        \    synonym for 'nhm-tool plan [-d dir] deps project-name'"
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
                    normal normal Nothing False
            case distData of
                Nothing -> usage (Just HelpDist) False
                Just (normalizeDistData -> distData'@DistData {..}) ->
                    if | distDataHelp ->
                             usage (Just HelpDist) $ length args' == 1
                       | isJust distDataWaitArg
                         || null distDataTargetLib ->
                             usage (Just HelpDist) False
                       | otherwise -> cmdDist distData'
        "plan" : args' -> do
            let planData = foldl parsePlanArg (Just defaultArgs) args'
                defaultArgs = PlanData "" Nothing Nothing False False
            case planData of
                Nothing -> usage (Just HelpPlan) False
                Just planData'@PlanData {..} ->
                    if | planDataHelp ->
                             usage (Just HelpPlan) $ length args' == 1
                       | isJust planDataWaitArg
                         || planDataWaitProjectName ->
                             usage (Just HelpPlan) False
                       | otherwise -> cmdPlan planData'
        "deps" : args' -> do
            let planData = foldl parsePlanArg (Just defaultArgs) args'
                defaultArgs = PlanData "" Nothing Nothing True False
            case planData of
                Nothing -> usage (Just HelpDeps) False
                Just planData'@PlanData {..} ->
                    if | planDataHelp ->
                             usage (Just HelpDeps) $ length args' == 1
                       | isJust planDataWaitArg
                         || planDataWaitProjectName ->
                             usage (Just HelpDeps) False
                       | otherwise -> cmdPlan planData'
        "init" : args' -> do
            let initData = foldl parseInitArg (Just defaultArgs) args'
                defaultArgs = InitData defaultInitDataPrefix False False False
                    "" ["-Wall", "-O2"] Nothing False
            case initData of
                Nothing -> usage (Just HelpInit) False
                Just initData'@InitData {..} ->
                    if | initDataHelp ->
                             usage (Just HelpInit) $ length args' == 1
                       | isJust initDataWaitArg
                         || null initDataProject
                         || initDataForce && initDataToStdout ->
                             usage (Just HelpInit) False
                       | otherwise -> cmdInit initData'
        [arg] | arg `elem` ["-h", "-help", "--help"] ->
                  usage Nothing True
              | arg `elem` ["-v", "-version", "--version"] ->
                  putStrLn progVersion >> exitSuccess
        args' -> usage Nothing $ null args'
    where normalizeDistData dist@DistData { distDataDir = "-" } =
              normalizeDistData dist { distDataDir = defaultDistDataDir
                                     , distDataPatchOnly = True
                                     }
          normalizeDistData dist@DistData { distDataArchive = "-" } =
              normalizeDistData dist { distDataArchive = "" }
          normalizeDistData dist@DistData { distDataTargetDir = "-" } =
              normalizeDistData dist { distDataTargetDir = "" }
          normalizeDistData dist@DistData { distDataDir = "" } =
              normalizeDistData dist { distDataDir = defaultDistDataDir }
          normalizeDistData dist = dist

parseDistArg :: Maybe DistData -> String -> Maybe DistData
parseDistArg Nothing _ = Nothing
parseDistArg (Just dist@DistData {..}) arg =
    case distDataWaitArg of
        Nothing ->
            let (opt, value) = splitAt 2 arg
            in if | "-d" == opt ->
                        Just $ if null value
                                   then dist { distDataWaitArg = Just opt }
                                   else dist' { distDataDir = value }
                  | "-t" == opt ->
                        Just $ if null value
                                   then dist { distDataWaitArg = Just opt }
                                   else dist' { distDataTargetDir = value }
                  | "-a" == opt ->
                        Just $ if null value
                                   then dist { distDataWaitArg = Just opt }
                                   else dist' { distDataArchive = value }
                  | "-p" == arg ->
                        Just dist' { distDataPatchOnly = True }
                  | "-v" == arg ->
                        Just dist' { distDataOwnVerbosity = verbose }
                  | "-vv" == arg ->
                        Just dist' { distDataOwnVerbosity = verbose
                                   , distDataOtherVerbosity = verbose
                                   }
                  | (`elem` ["-h", "-help", "--help"]) arg ->
                        Just dist' { distDataHelp = True }
                  | Just '-' == (fst <$> uncons arg) ->
                        Nothing
                  | null distDataTargetLib ->
                        Just dist' { distDataTargetLib = arg }
                  | otherwise ->
                        Nothing
        Just "-d" -> Just dist' { distDataDir = arg }
        Just "-t" -> Just dist' { distDataTargetDir = arg }
        Just "-a" -> Just dist' { distDataArchive = arg }
        Just _ -> undefined
    where dist' = dist { distDataWaitArg = Nothing }

parsePlanArg :: Maybe PlanData -> String -> Maybe PlanData
parsePlanArg Nothing _ = Nothing
parsePlanArg (Just plan@PlanData {..}) arg =
    case planDataWaitArg of
        Nothing ->
            let (opt, value) = splitAt 2 arg
            in if | "deps" == arg && not hasDataQuery ->
                        Just plan' { planDataWaitProjectName = True }
                  | "abi" == arg && not hasDataQuery ->
                        Just plan' { planDataQuery = Just PlanDataQueryAbi }
                  | "-d" == opt ->
                        Just $ if null value
                                   then plan { planDataWaitArg = Just opt }
                                   else plan' { planDataBuilddir = value }
                  | (`elem` ["-h", "-help", "--help"]) arg ->
                        Just plan' { planDataHelp = True }
                  | Just '-' == (fst <$> uncons arg) ->
                        Nothing
                  | planDataWaitProjectName ->
                        Just plan'
                            { planDataQuery = Just $ PlanDataQueryDeps arg
                            , planDataWaitProjectName = False
                            }
                  | otherwise ->
                        Nothing
        Just "-d" -> Just plan' { planDataBuilddir = arg }
        Just _ -> undefined
    where plan' = plan { planDataWaitArg = Nothing }
          hasDataQuery = isJust planDataQuery || planDataWaitProjectName

parseInitArg :: Maybe InitData -> String -> Maybe InitData
parseInitArg Nothing _ = Nothing
parseInitArg (Just init'@InitData {..}) arg =
    case initDataWaitArg of
        Nothing ->
            let (opt, value) = splitAt 2 arg
            in if | "-p" == opt ->
                        Just $ if null value
                                   then init' { initDataWaitArg = Just opt }
                                   else init'' { initDataPrefix = value }
                  | "-no-threaded" == arg ->
                        Just init'' { initDataNoThreaded = True }
                  | "-f" == arg ->
                        Just init'' { initDataForce = True }
                  | "-to-stdout" == arg ->
                        Just init'' { initDataToStdout = True }
                  | (`elem` ["-h", "-help", "--help"]) arg ->
                        Just init'' { initDataHelp = True }
                  | Just '-' == (fst <$> uncons arg) ->
                        Nothing
                  | null initDataProject ->
                        Just init'' { initDataProject = arg }
                  | otherwise ->
                        Nothing
        Just "-p" -> Just init'' { initDataPrefix = arg }
        Just _ -> undefined
    where init'' = init' { initDataWaitArg = Nothing }

cmdDist :: DistData -> IO ()
cmdDist DistData {..} = do
    patchelf <- requireProgram' $ simpleProgram "patchelf"
    if distDataPatchOnly
        then patchTargetLib patchelf
        else do
            ldd <- requireProgram' $ simpleProgram "ldd"
            putStrLn' "---> Collecting libraries"
            lddOut <- getProgramOutput' ldd [distDataTargetLib]
            case parseLddOutput lddOut of
                Left err -> do
                    hPutStrLn stderr $ show err ++ " in\n" ++ lddOut
                    exitFailure
                Right recs -> do
                    tar' <- requireProgram' tarProgram
                    collectLibs recs lddOut
                    unless (null distDataTargetDir) $
                        putStrLn' "" >> patchTargetLib patchelf
                    unless (null distDataArchive) $
                        putStrLn' "" >> archiveLibs tar'
    where patchTargetLib patchelf = unless (null distDataTargetDir) $ do
              putStrLn' $ "---> Patching " ++ distDataTargetLib
              let printRpath = getProgramOutput'
                      patchelf ["--print-rpath", distDataTargetLib]
              patchelfOut <- printRpath
              case parsePatchelfRpathOutput patchelfOut of
                  Left err -> do
                      hPutStrLn stderr $ show err ++ " in\n" ++ patchelfOut
                      exitFailure
                  Right paths -> do
                      unless (distDataTargetDir `elem` paths) $ runProgram'
                          patchelf ["--set-rpath"
                                   ,distDataTargetDir ++ ':' : patchelfOut
                                   ,distDataTargetLib
                                   ]
                      printRpath >>= putStrLnTrim
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
          archiveLibs tar' = unless (null distDataArchive) $ do
              putStrLn' "---> Archiving artifacts"
              tarOut <- getProgramOutput'
                  tar' ["czvf"
                       ,distDataArchive <.> ".tar.gz"
                       ,distDataTargetLib
                       ,distDataDir
                       ]
              putStrLnTrim tarOut
          requireProgram' = fmap fst .
              flip (requireProgram distDataOtherVerbosity) emptyProgramDb
          getProgramOutput' = getProgramOutput distDataOtherVerbosity
          runProgram' = runProgram distDataOtherVerbosity
          putStrLn' = when (distDataOwnVerbosity == verbose) . putStrLn
          putStrLnTrim = putStrLn' . dropWhileEnd (== '\n')

parsePatchelfRpathOutput :: String -> Either ParseError [String]
parsePatchelfRpathOutput =
    parse (many (satisfy (/= colon)) `sepBy` char colon) "patchelf_rpath"
    where colon = ':'

parseLddOutput :: String -> Either ParseError [LddRec]
parseLddOutput = flip parse "ldd" $ manyLines $
    try (toLddRec <$>
             anyCharsTill arrow <*>
                 ((Just .) . (:) <$> char '/' <*> right
                  <|> Nothing <$ string "not found"
                 )
        )
    <|> (`LibOther` Nothing) <$> (try right <|> anyCharsTill (arrow *> addr))
    where manyLines = many . (spaces *>) . (<* newline)
          toLddRec lib | "libHS" `isPrefixOf` lib = LibHS lib
                       | "libffi.so" `isPrefixOf` lib = LibFFI lib
                       | otherwise = LibOther lib
          anyCharsTill = manyTill $ satisfy (/= '\n')
          right = anyCharsTill $ spaces1 *> addr
          addr = string "(0x" *> many1 hexDigit *> char ')'
          arrow = spaces1 *> string "=>" *> spaces1
          spaces1 = skipMany1 space

cmdPlan :: PlanData -> IO ()
cmdPlan PlanData {..} = do
    let buildDir | null planDataBuilddir = ProjectRelativeToDir "."
                 | otherwise = InBuildDir planDataBuilddir
    path <- findPlanJson buildDir
    case planDataQuery of
        Nothing ->
            readFile path >>= putStrLn
        Just (PlanDataQueryDeps proj) -> do
            units <- pjUnits <$> decodePlanJson path
            let comps = [ uComps
                        | Unit {..} <- M.elems units
                        , uType == UnitTypeLocal
                        , let uPkgName = (\(PkgId (PkgName pkg) _) -> pkg) uPId
                        , uPkgName == T.pack proj
                        ]
            when (null comps) $ do
                hPutStrLn stderr $ "Failed to find plan for " ++ proj
                exitFailure
            let deps = foldl (\a curComps ->
                                  let libComps = M.filterWithKey
                                          (const . (== CompNameLib)) curComps
                                  in M.foldr S.union a $
                                      M.map ciLibDeps libComps
                             ) S.empty comps
            forM_ (S.toList deps) $ \(UnitId unit) ->
                putStrLn $ "package-id " ++ T.unpack unit
        Just PlanDataQueryAbi -> do
            PlanJson {..} <- decodePlanJson path
            T.putStrLn $
                T.intercalate "-" [pjArch, pjOs, dispPkgId pjCompilerId]

cmdInit :: InitData -> IO ()
cmdInit init'@InitData {..} = do
    let files = [("cabal.project", cabalProject init', True)
                ,("Setup.hs", setupHs init', True)
                ,(initDataProject ++ ".cabal", projectCabal init', True)
                ,("Makefile", makefile init', True)
                ,(replaceChar '-' '_' initDataProject ++ ".hs"
                 ,projectHs init'
                 ,False
                 )
                ,("hie.yaml", hieYaml init', True)
                ]
    forM_ files $ \(name, file, overridable) ->
        if initDataToStdout
            then printHeader (name ++ "\n") >> T.putStrLn file
            else do
                exists <- doesFileExist name
                if exists
                    then if initDataForce && overridable
                             then T.writeFile name file
                             else hPutStrLn stderr $
                                 if overridable
                                     then useForceMsg name
                                     else existsMsg name
                    else T.writeFile name file
    where printHeader header = do
              isANSITerm <- hSupportsANSI stdout
              if isANSITerm
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
     \  ghc-options:             ", T.pack $ unwords initDataGhcOptions, "\n"
    ,if initDataNoThreaded
         then ""
         else
     "\n\
     \  if impl(ghc >= 9.0.1)\n\
     \    ghc-options:           -threaded\n"
    ]

makefile :: InitData -> Text
makefile InitData {..} = T.concat
    ["NAME := ", T.pack $ replaceChar '-' '_' initDataProject, "\n\
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
     \DISTR := $(PKGNAME)-$(PKGVER).tar.gz\n\
     \\n\
     \override OBJS := $(SRC:.hs=.o)\n\
     \override HIOBJS := $(SRC:.hs=.hi)\n\
     \override DYNOBJS := $(SRC:.hs=.dyn_o)\n\
     \override DYNHIOBJS := $(SRC:.hs=.dyn_hi)\n\
     \override STUBS := $(SRC:.hs=_stub.h)\n\
     \\n\
     \GHC := ghc\n\
     \CABAL := cabal\n\
     \GHCVER := $(shell $(GHC) --numeric-version)\n\
     \CABALVER := $(shell $(CABAL) --numeric-version)\n\
     \GHCPUID := $(shell \\\n\
     \    if printf '3.12\\n$(CABALVER)' | sort -VC && \\\n\
     \       printf '9.10\\n$(GHCVER)' | sort -VC; \\\n\
     \    then ghcpuid=`$(GHC) --info | sed -n \\\n\
     \             's/^.*\\x28\"Project Unit Id\",\"\\(.*\\)\"\\x29.*$$/\
     \\\1/ip'`; \\\n\
     \        if test -z \"$$ghcpuid\"; \\\n\
     \        then echo \"ghc-$(GHCVER)\"; \\\n\
     \        else echo \"$$ghcpuid\"; \\\n\
     \        fi; \\\n\
     \    else echo \"ghc-$(GHCVER)\"; \\\n\
     \    fi)\n\
     \GHCENV := .ghc.environment.$(MACHINE)-$(KERNEL)-$(GHCVER)\n\
     \GHCENVLNK := ", ghcEnvLnk, "\n\
     \DEPLIBS := $(MACHINE)-$(KERNEL)-$(GHCPUID)\n\
     \BUILDDIR := dist-nhm\n\
     \SETUPCONFIG := $(BUILDDIR)/setup-config\n\
     \\n\
     \.PHONY: all env config install clean clean-all\n\
     \\n\
     \all: $(DISTR)\n\
     \\n\
     \env: $(GHCENVLNK)\n\
     \\n\
     \config: $(SETUPCONFIG)\n\
     \\n\
     \$(GHCENVLNK): cabal.project $(PKGNAME).cabal\n\
     \\trm -f \"$(GHCENVLNK)\"\n\
     \\t$(CABAL) install --builddir=\"$(BUILDDIR)\" --lib --only-dependencies \
     \\\\n\
     \\t  --package-env .\n\
     \\tsed -i 's/\\(^package-id \\)/--\\1/' \"$(GHCENV)\"\n",
     updatePath,
     "\t$(NHMTOOL) deps \"$(PKGNAME)\" -d \"$(BUILDDIR)\" >> \"$(GHCENV)\"\n\
     \\tln -sf \"$(GHCENV)\" \"$(GHCENVLNK)\"\n\
     \\n\
     \$(SETUPCONFIG): $(GHCENVLNK)\n",
     updatePath,
     "\trunhaskell --ghc-arg=-package=base \\\n\
     \\t  --ghc-arg=-package=\"$(PKGDISTR)\" Setup.hs configure \\\n\
     \\t  --builddir=\"$(BUILDDIR)\" \\\n\
     \\t  --package-db=clear --package-db=global \\\n\
     \\t  $$(sed -n 's/^\\(package-db\\)\\s\\+/--\\1=/p' \"$(GHCENV)\") \\\n\
     \\t  $$(sed -n 's/^package-id\\s\\+\\(.*\\)'` \\\n\
     \\t    `'\\(-\\([0-9]\\+\\.\\)*[0-9]\\+\\($$\\|-.*\\)\\)/'` \\\n\
     \\t    `'--dependency=\\1=\\1\\2/p' \\\n\
     \\t    \"$(GHCENV)\") \\\n\
     \\t  --prefix=\"$(PREFIX)\"\n\
     \\n\
     \$(DISTR): $(SETUPCONFIG) $(SRC)\n",
     updatePath,
     "\trunhaskell --ghc-arg=-package=base \\\n\
     \\t  --ghc-arg=-package=\"$(PKGDISTR)\" Setup.hs build \\\n\
     \\t  --builddir=\"$(BUILDDIR)\" \\\n\
     \\t  --ghc-options=\"$(SRC) -o $(LIB) $(LINKRTS)\"\n\
     \\n\
     \install: $(DISTR)\n\
     \\tinstall -d \"$(PREFIX)\"\n\
     \\ttar xf \"$(DISTR)\" -C \"$(PREFIX)\" --no-same-owner\n\
     \\n\
     \clean:\n\
     \\trm -rf \"$(DEPLIBS)\"\n\
     \\trm -f $(OBJS) $(HIOBJS) $(DYNOBJS) $(DYNHIOBJS) $(STUBS)\n\
     \\trm -f \"$(LIB)\"\n\
     \\n\
     \clean-all: clean\n\
     \\trm -rf \"$(BUILDDIR)\"\n\
     \\trm -f \"$(GHCENV)\" \"$(GHCENVLNK)\" \"$(DISTR)\"\n"
    ]
    where updatePath =
              "\tif test \"$(NHMTOOL)\" = nhm-tool && ! command -v nhm-tool \
              \>/dev/null; \\\n\
              \\tthen \\\n\
              \\t  PATH=$$(dirname $$($(CABAL) list-bin \"$(PKGDISTR)\" \\\n\
              \\t    --builddir=\"$(BUILDDIR)\")):$$PATH; \\\n\
              \\tfi; \\\n"

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

hieYaml :: InitData -> Text
hieYaml InitData {..} = T.concat
    ["cradle:\n\
     \  bios:\n\
     \    shell: |\n\
     \      if [ -L ", ghcEnvLnk, " ]\n\
     \      then\n\
     \          printf \""
    ,T.replace "-" "\\055" $ T.pack $ intercalate "\\n" initDataGhcOptions
    ,"\\n\" > \"$HIE_BIOS_OUTPUT\"\n\
     \          find . -name '[A-Z]*.hs' -exec sh -c '\n\
     \              file=$(echo \"$1\" | cut -c 3-)\n\
     \              if grep -q \"^\\s*module $(echo \"$file\" |\n\
     \                  sed '\\''s/^\\(.*\\)\\..*/\\1/;s/\\//./g'\\'')\" \
     \\"$file\"\n\
     \              then\n\
     \                  echo \"$file\" >> \"$HIE_BIOS_OUTPUT\"\n\
     \              fi\n\
     \          ' sh {} \\;\n\
     \      else\n\
     \          echo \"Ghc environment file wasn't found, \"`\n\
     \              `\"run \\\"make env\\\" and restart \
     \language server.\" >&2\n\
     \          exit 1\n\
     \      fi\n"
    ]

ghcEnvLnk :: Text
ghcEnvLnk = ".ghc.environment.lnk"

