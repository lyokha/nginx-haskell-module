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

data DepsData = DepsData { depsDataBuilddir :: String
                         , depsDataProject :: String
                         , depsDataWaitArg :: Maybe String
                         , depsDataHelp :: Bool
                         }

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
        \  * nhm-tool deps [-d dir] project-name\n\n\
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
        "deps" : args' -> do
            let depsData = foldl parseDepsArg (Just defaultArgs) args'
                defaultArgs = DepsData "" "" Nothing False
            case depsData of
                Nothing -> usage (Just HelpDeps) False
                Just depsData'@DepsData {..} ->
                    if | depsDataHelp ->
                             usage (Just HelpDeps) $ length args' == 1
                       | isJust depsDataWaitArg
                         || null depsDataProject ->
                             usage (Just HelpDeps) False
                       | otherwise -> cmdDeps depsData'
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

parseDepsArg :: Maybe DepsData -> String -> Maybe DepsData
parseDepsArg Nothing _ = Nothing
parseDepsArg (Just deps@DepsData {..}) arg =
    case depsDataWaitArg of
        Nothing ->
            let (opt, value) = splitAt 2 arg
            in if | "-d" == opt ->
                        Just $ if null value
                                   then deps { depsDataWaitArg = Just opt }
                                   else deps' { depsDataBuilddir = value }
                  | (`elem` ["-h", "-help", "--help"]) arg ->
                        Just deps' { depsDataHelp = True }
                  | Just '-' == (fst <$> uncons arg) ->
                        Nothing
                  | null depsDataProject ->
                        Just deps' { depsDataProject = arg }
                  | otherwise ->
                        Nothing
        Just "-d" -> Just deps' { depsDataBuilddir = arg }
        Just _ -> undefined
        where deps' = deps { depsDataWaitArg = Nothing }

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
    let pdb = emptyProgramDb
        patchelf = simpleProgram "patchelf"
    (patchelf', pdb') <- requireProgram distDataOtherVerbosity patchelf pdb
    if distDataPatchOnly
        then patchTargetLib patchelf'
        else do
            let ldd = simpleProgram "ldd"
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
                    unless (null distDataTargetDir) $
                        putStrLn' "" >> patchTargetLib patchelf'
                    unless (null distDataArchive) $
                        putStrLn' "" >> archiveLibs tar'
    where patchTargetLib patchelf'
              | null distDataTargetDir = return ()
              | otherwise = do
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
          archiveLibs tar'
              | null distDataArchive = return ()
              | otherwise = do
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
     \DISTR := $(PKGNAME)-$(PKGVER).tar.gz\n\
     \\n\
     \OBJS := $(SRC:.hs=.o)\n\
     \HIOBJS := $(SRC:.hs=.hi)\n\
     \DYNOBJS := $(SRC:.hs=.dyn_o)\n\
     \DYNHIOBJS := $(SRC:.hs=.dyn_hi)\n\
     \STUBS := $(SRC:.hs=_stub.h)\n\
     \\n\
     \GHC := ghc\n\
     \CABAL := cabal\n\
     \GHCVER := $(shell $(GHC) --numeric-version)\n\
     \GHCENV := .ghc.environment.$(MACHINE)-$(KERNEL)-$(GHCVER)\n\
     \GHCENVLNK := ", ghcEnvLnk, "\n\
     \DEPLIBS := $(MACHINE)-$(KERNEL)-ghc-$(GHCVER)\n\
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
     \\trm -f $(GHCENVLNK)\n\
     \\t$(CABAL) install --builddir=\"$(BUILDDIR)\" --lib --only-dependencies \
     \\\\n\
     \\t  --package-env .\n\
     \\tsed -i 's/\\(^package-id \\)/--\\1/' $(GHCENV)\n",
     updatePath,
     "\t$(NHMTOOL) deps $(PKGNAME) -d \"$(BUILDDIR)\" >> $(GHCENV)\n\
     \\tln -sf $(GHCENV) $(GHCENVLNK)\n\
     \\n\
     \$(SETUPCONFIG): $(GHCENVLNK)\n",
     updatePath,
     "\trunhaskell --ghc-arg=-package=base \\\n\
     \\t  --ghc-arg=-package=$(PKGDISTR) Setup.hs configure \\\n\
     \\t  --builddir=\"$(BUILDDIR)\" \\\n\
     \\t  --package-db=clear --package-db=global \\\n\
     \\t  $$(sed -n 's/^\\(package-db\\)\\s\\+/--\\1=/p' $(GHCENV)) \\\n\
     \\t  $$(sed -n 's/^package-id\\s\\+\\(.*\\)'` \\\n\
     \\t    `'\\(-\\([0-9]\\+\\.\\)*[0-9]\\+\\($$\\|-.*\\)\\)/'` \\\n\
     \\t    `'--dependency=\\1=\\1\\2/p' \\\n\
     \\t    $(GHCENV)) \\\n\
     \\t  --prefix=$(PREFIX)\n\
     \\n\
     \$(DISTR): $(SETUPCONFIG) $(SRC)\n",
     updatePath,
     "\trunhaskell --ghc-arg=-package=base \\\n\
     \\t  --ghc-arg=-package=$(PKGDISTR) Setup.hs build \\\n\
     \\t  --builddir=\"$(BUILDDIR)\" \\\n\
     \\t  --ghc-options=\"$(SRC) -o $(LIB) $(LINKRTS)\"\n\
     \\n\
     \install: $(DISTR)\n\
     \\tinstall -d $(PREFIX)\n\
     \\ttar xf $(DISTR) -C $(PREFIX) --no-same-owner\n\
     \\n\
     \clean:\n\
     \\trm -rf $(DEPLIBS)\n\
     \\trm -f $(OBJS) $(HIOBJS) $(DYNOBJS) $(DYNHIOBJS) $(STUBS)\n\
     \\trm -f $(LIB)\n\
     \\n\
     \clean-all: clean\n\
     \\trm -rf $(BUILDDIR)\n\
     \\trm -f $(GHCENV) $(GHCENVLNK) $(DISTR)\n"
    ]
    where updatePath =
              "\tif test \"$(NHMTOOL)\" = nhm-tool && ! command -v nhm-tool \
              \>/dev/null; \\\n\
              \\tthen \\\n\
              \\t  PATH=$$(dirname $$($(CABAL) list-bin $(PKGDISTR) \\\n\
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

