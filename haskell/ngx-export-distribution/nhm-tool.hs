{-# LANGUAGE RecordWildCards, LambdaCase, MultiWayIf, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Parsec
import Distribution.Simple.Program.Types
import Distribution.Simple.Program
import Distribution.Verbosity
import Cabal.Plan
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Functor
import Data.Maybe
import Data.Char
import Data.List
import Data.List.Extra
import Prettyprinter (pretty, annotate)
import Prettyprinter.Render.Terminal
import System.Environment
import System.Directory
import System.FilePath
import System.Exit
import System.IO
import GHC.IO.Device
import qualified GHC.IO.FD as FD

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

data DepsData = DepsData { depsDataProject :: String
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
            | LibOther String (Maybe FilePath)
            deriving Show

data HelpSection = HelpDist | HelpDeps | HelpInit deriving Eq

usage :: Maybe HelpSection -> Bool -> IO ()
usage section success = do
    when (isNothing section) $
        putStrLn "nhm-tool: help building custom Haskell handlers for Nginx\n\
                 \this is a tool from \
                 \https://github.com/lyokha/nginx-haskell-module\n"
    putStrLn "Usage:"
    when (isNothing section || section == Just HelpDist) $
        T.putStrLn $ T.concat ["\n\
        \  * nhm-tool dist [-d dir -a ar | -p] [-t target_dir] [-v | -vv] \
        \hslib\n\n\
        \    'dir' is a directory where dependent libraries will be\n\
        \      collected (default is", T.pack defaultDistDataDir, ")\n\
        \    'target_dir' is a directory where dependent libraries will be\n\
        \      installed (no default, 'hslib' will not be patched if omitted)\n\
        \    'ar' is the base name of the archive to contain 'hslib' and\n\
        \      'dir' (no default, the archive will not be created if omitted)\n\
        \    if '-p' (patch-only) is specified then dependent libraries will\n\
        \      neither be collected nor archived\n\
        \    use options '-v' and '-vv' to increase verbosity level\n\
        \    special value '-' for 'dir', 'target_dir', and 'ar' resets them\n\
        \      (with -d- being equivalent to -p)"
        ]
    when (isNothing section || section == Just HelpDeps) $
        T.putStrLn "\n\
        \  * nhm-tool deps project-name\n\n\
        \    print all direct dependencies of the given project,\n\
        \    the output is compatible with the format of GHC environment files"
    when (isNothing section || section == Just HelpInit) $
        T.putStrLn $ T.concat ["\n\
        \  * nhm-tool init [-p dir] [-no-threaded] [-f | -to-stdout] \
        \project-name\n\n\
        \    '-p' prefix for install dir, default is ",
        T.pack defaultInitDataPrefix, "\n\
        \    '-no-threaded' use base RTS library\n\
        \    '-f' force re-writing of existing files (except the source file)\n\
        \    '-to-stdout' do not write files but dump what will be written"
        ]
    when (isNothing section) $
        T.putStrLn "\n\
        \  * nhm-tool [-h | -help | --help]\n\n\
        \    show this help message and exit, applicable in sub-commands \
        \as well"
    if success
        then exitSuccess
        else exitFailure

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ usage Nothing True
    case head args of
        "dist" -> do
            let distData = foldl parseDistArgs (Just defaultArgs) $ tail args
                defaultArgs = DistData defaultDistDataDir "" "" False ""
                    Nothing normal normal False
            case distData of
                Nothing -> usage (Just HelpDist) False
                Just (normalizeDistData -> distData'@DistData {..}) ->
                    if distDataHelp
                        then usage (Just HelpDist) True
                        else if isJust distDataWait || null distDataTargetLib
                                 then usage (Just HelpDist) False
                                 else cmdDist distData'
        "deps" -> do
            let depsData = foldl parseDepsArgs (Just defaultArgs) $ tail args
                defaultArgs = DepsData "" False
            case depsData of
                Nothing -> usage (Just HelpDeps) False
                Just depsData'@DepsData {..} ->
                    if depsDataHelp
                        then usage (Just HelpDeps) True
                        else if null depsDataProject
                                 then usage (Just HelpDeps) False
                                 else cmdDeps depsData'
        "init" -> do
            let initData = foldl parseInitArgs (Just defaultArgs) $ tail args
                defaultArgs = InitData defaultInitDataPrefix False False False
                    "" Nothing False
            case initData of
                Nothing -> usage (Just HelpInit) False
                Just initData'@InitData {..} ->
                    if initDataHelp
                        then usage (Just HelpInit) True
                        else if isJust initDataWait || null initDataProject ||
                                    initDataForce && initDataToStdout
                                 then usage (Just HelpInit) False
                                 else cmdInit initData'
        "-h" -> usage Nothing True
        "-help" -> usage Nothing True
        "--help" -> usage Nothing True
        _ -> usage Nothing False
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

parseDistArgs :: Maybe DistData -> String -> Maybe DistData
parseDistArgs Nothing _ = Nothing
parseDistArgs (Just dist@DistData {..}) arg =
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
                  | otherwise ->
                         if null distDataTargetLib
                             then Just dist' { distDataTargetLib = arg }
                             else Nothing
        Just ArgWaitD -> Just dist' { distDataDir = arg }
        Just ArgWaitT -> Just dist' { distDataTargetDir = arg }
        Just ArgWaitA -> Just dist' { distDataArchive = arg }
        Just _ -> undefined
        where dist' = dist { distDataWait = Nothing }

parseDepsArgs :: Maybe DepsData -> String -> Maybe DepsData
parseDepsArgs Nothing _ = Nothing
parseDepsArgs (Just deps@DepsData {..}) arg =
    if | "-h" == arg || "-help" == arg || "--help" == arg ->
             Just deps { depsDataHelp = True }
       | "-" `isPrefixOf` arg -> Nothing
       | otherwise ->
              if null depsDataProject
                  then Just deps { depsDataProject = arg }
                  else Nothing

parseInitArgs :: Maybe InitData -> String -> Maybe InitData
parseInitArgs Nothing _ = Nothing
parseInitArgs (Just init'@InitData {..}) arg =
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
                  | otherwise ->
                         if null initDataProject
                             then Just init'' { initDataProject = arg }
                             else Nothing
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
                Left msg -> do
                    hPrint stderr msg
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
                      Left msg -> do
                          hPrint stderr msg
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
              let recsLibHS = filter (\case
                                          LibHS _ _ -> True
                                          _ -> False
                                     ) recs
              if null recsLibHS
                  then do
                      hPutStrLn stderr
                          "There were no Haskell libraries collected"
                      exitFailure
                  else do
                      let recsLibHSNotFound =
                              filter (\case
                                          LibHS _ Nothing -> True
                                          _ -> False
                                     ) recsLibHS
                      if null recsLibHSNotFound
                          then do
                              createDirectoryIfMissing False distDataDir
                              forM_ recsLibHS $ \case
                                  LibHS _ (Just path) -> do
                                      let dst = distDataDir </>
                                                    takeFileName path
                                      putStrLn' $ path ++ " -> " ++ dst
                                      copyFile path dst
                                  _ -> undefined
                          else do
                              hPutStrLn stderr $
                                 "There were missing Haskell libraries:\n" ++
                                     lddOut
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
          putStrLnTrim = putStrLn' . fst . spanEnd (== '\n')

parsePatchelfRpathOutput :: String -> Either ParseError [String]
parsePatchelfRpathOutput =
    parse (many (satisfy (/= colon)) `sepBy` char colon) "patchelf_rpath"
    where colon = ':'

parseLddOutput :: String -> Either ParseError [LddRec]
parseLddOutput = flip parse "ldd" $ many $
    spaces >>
    (try (do
              lib <- manyTill anyChar' sep
              path <- (char bs >> right <&> Just . (bs :))
                      <|> (string "not found" >> return Nothing)
              return $ if "libHS" `isPrefixOf` lib
                           then LibHS lib path
                           else LibOther lib path
         )
     <|> (right <&> (`LibOther` Nothing))
    )
    where right = manyTill anyChar' (try $ spaces1 >> addr >> newline)
          addr = string "(0x" >> many1 hexDigit >> char ')'
          anyChar' = satisfy (/= '\n')
          sep = spaces1 >> string "=>" >> spaces1
          spaces1 = skipMany1 space
          bs = '/'

cmdDeps :: DepsData -> IO ()
cmdDeps DepsData {..} = do
    units <- pjUnits <$> findAndDecodePlanJson (ProjectRelativeToDir ".")
    let locals = [ Unit {..}
                 | Unit {..} <- M.elems units
                 , uType == UnitTypeLocal
                 , M.member CompNameLib uComps
                 , toPkgName uPId == T.pack depsDataProject
                 ]
    when (null locals) $ do
        hPutStrLn stderr $ "Failed to find plan for " ++ depsDataProject
        exitFailure
    let locals' = foldl (\a (Unit {..}) ->
                            let comps = M.filterWithKey
                                    (const . (== CompNameLib)) uComps
                                deps = M.map ciLibDeps comps
                            in M.foldr S.union a deps
                        ) S.empty locals
    forM_ (S.toList locals') $ \(UnitId local) ->
        putStrLn $ "package-id " ++ T.unpack local
    where toPkgName (PkgId (PkgName name) _) = name

cmdInit :: InitData -> IO ()
cmdInit init'@InitData {..} = do
    let files = [("cabal.project", cabalProject init', True)
                ,("Setup.hs", setupHs init', True)
                ,(initDataProject ++ ".cabal", projectCabal init', True)
                ,("Makefile", makefile init', True)
                ,(replace "-" "_" initDataProject ++ ".hs"
                  ,projectHs init'
                  ,False
                 )
                ]
    forM_ files $
        if initDataToStdout
            then \(name, file, _) ->
                printHeader name >> T.putStrLn file
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
    where printHeader header = do
              isTerm <- isTerminal FD.stdout
              if isTerm
                  then putDoc $ annotate (color Blue <> underlined) $
                      pretty $ header ++ "\n\n"
                  else putStrLn $ " ~~~ " ++ header ++ "\n"
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
     \cabal-version:              1.24\n\
     \\n\
     \custom-setup\n\
     \  setup-depends:            base >= 4.8 && < 5\n\
     \                          , ngx-export-distribution\n\
     \\n\
     \library\n\
     \  default-language:         Haskell2010\n\
     \  build-depends:            base >= 4.8 && < 5\n\
     \                          , ngx-export\n\
     \\n\
     \  ghc-options:             -Wall -O2\n"
    ,if initDataNoThreaded
         then ""
         else
     "\n\
     \  if impl(ghc >= 8.10.6)\n\
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
     \\n\
     \SRC := $(NAME).hs\n\
     \LIB := $(NAME).so\n\
     \STUB := $(NAME)_stub.h\n\
     \DISTR := $(PKGNAME)-$(PKGVER).tar.gz\n\
     \\n\
     \GHCVER := $(shell ghc --numeric-version)\n\
     \GHCENV := .ghc.environment.$(MACHINE)-$(KERNEL)-$(GHCVER)\n\
     \DEPLIBS := $(MACHINE)-$(KERNEL)-ghc-$(GHCVER)\n\
     \DISTDIR := dist\n\
     \DISTV2DIR := dist-newstyle\n\
     \\n\
     \all: $(DISTR)\n\
     \\n\
     \$(DISTR): $(SRC)\n\
     \\tcabal install --lib --only-dependencies --package-env .\n\
     \\tsed -i 's/\\(^package-id \\)/--\\1/' $(GHCENV)\n\
     \\trunhaskell --ghc-arg=-package=base \\\n\
     \\t --ghc-arg=-package=$(PKGDISTR) Setup.hs configure \\\n\
     \\t --package-db=clear --package-db=global \\\n\
     \\t $$(sed -n 's/^\\(package-db\\)\\s\\+/--\\1=/p' $(GHCENV)) \\\n\
     \\t --prefix=$(PREFIX)\n\
     \\tnhm-tool deps $(PKGNAME) >> $(GHCENV)\n\
     \\trunhaskell --ghc-arg=-package=base \\\n\
     \\t --ghc-arg=-package=$(PKGDISTR) Setup.hs build \\\n\
     \\t --ghc-options=\"$(SRC) -o $(LIB) $(LINKRTS)\"\n\
     \\n\
     \install: $(DISTR)\n\
     \\tinstall -d $(PREFIX)\n\
     \\ttar xf $(DISTR) -C $(PREFIX) --no-same-owner\n\
     \\n\
     \.PHONY: clean\n\
     \\n\
     \clean:\n\
     \\trm -rf $(DISTDIR) $(DISTV2DIR) $(DEPLIBS)\n\
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
    ,T.pack $ fst $
        foldl (\(a, tr) v -> if v == '-' || v == '_'
                                 then (a, True)
                                 else if tr || null a
                                          then (a ++ [toUpper v], False)
                                          else (a ++ [v], False)
              ) ("", False) initDataProject
    , " where\n\n"
    ]

