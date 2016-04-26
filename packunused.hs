{-# LANGUAGE CPP, RecordWildCards #-}

module Main where

import           Control.Monad
import           Data.IORef
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Maybe
import           Data.Monoid
import           Data.Version (Version(Version), showVersion)
import           Distribution.InstalledPackageInfo (exposedModules, installedPackageId)
#if MIN_VERSION_Cabal(1,21,0)
import           Distribution.InstalledPackageInfo (exposedName)
#endif
import           Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as MN
import           Distribution.Package (InstalledPackageId(..), packageId, pkgName)
import qualified Distribution.PackageDescription as PD
import           Distribution.Simple.Compiler
import           Distribution.Simple.Configure (localBuildInfoFile, getPersistBuildConfig, checkPersistBuildConfigOutdated)
#if MIN_VERSION_Cabal(1,21,0)
import           Distribution.Simple.Configure (tryGetPersistBuildConfig, ConfigStateFileError(..))
#endif
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PackageIndex (lookupInstalledPackageId)
import           Distribution.Simple.Utils (cabalVersion)
import           Distribution.Text (display)
import qualified Language.Haskell.Exts as H
import           Options.Applicative
import           Options.Applicative.Help.Pretty (Doc)
import qualified Options.Applicative.Help.Pretty as P
import           System.Directory (getModificationTime, getDirectoryContents, doesDirectoryExist, doesFileExist, getCurrentDirectory)
import           System.Exit (exitFailure)
import           System.FilePath ((</>), takeDirectory)
import           System.Process

import           Paths_packunused (version)

-- | CLI Options
data Opts = Opts
    { ignoreEmptyImports :: Bool
    , ignoreMainModule :: Bool
    , ignoredPackages :: [String]
    } deriving (Show)

opts :: Parser Opts
opts = Opts <$> switch (long "ignore-empty-imports" <> help "ignore empty .imports files")
            <*> switch (long "ignore-main-module" <> help "ignore Main modules")
            <*> many (strOption (long "ignore-package" <> metavar "PKG" <>
                                 help "ignore the specfied package in the report"))

helpFooter :: Doc
helpFooter = mconcat
    [ P.text "Tool to help find redundant build-dependencies in CABAL projects", P.linebreak
    , P.hardline
    , para $ "In order to use this tool you should set up the package to be analyzed as follows, " ++
             "before executing 'packunused':", P.linebreak

    , P.hardline
    , P.text "For stack:"
    , P.indent 2 $ P.vcat $ P.text <$>
      [ "stack clean"
      , "stack build --ghc-options=-ddump-minimal-imports"
      , "packunused"
      ]

    , P.hardline
    , P.text "For cabal:"
    , P.indent 2 $ P.vcat $ P.text <$>
      [ "cabal clean"
      , "rm *.imports        # (only needed for GHC<7.8)"
      , "cabal configure -O0 --disable-library-profiling"
      , "cabal build --ghc-option=-ddump-minimal-imports"
      , "packunused"
      ]
    , P.linebreak, P.hardline
    , P.text "Note:" P.<+> P.align
      (para $ "The 'cabal configure' command above only tests the default package configuration. " ++
              "You might need to repeat the process with different flags added to the 'cabal configure' step " ++
              "(such as '--enable-tests' or '--enable-benchmark' or custom cabal flags) " ++
              "to make sure to check all configurations")
    , P.linebreak, P.hardline
    , P.text "Report bugs to https://github.com/hvr/packunused/issues"
    ]
  where
    para = P.fillSep . map P.text . words

helpHeader :: String
helpHeader = "packunused " ++ showVersion version ++
             " (using Cabal "++ showVersion cabalVersion ++ ")"

chooseDistPref :: Bool -> IO String
chooseDistPref useStack = do
  if useStack
    then takeWhile (/= '\n') <$> readProcess "stack" (words "path --dist-dir") ""
    else return "dist"

getLbi :: Bool -> FilePath -> IO LocalBuildInfo
#if MIN_VERSION_Cabal(1,21,0)
getLbi useStack distPref = either explainError id <$> tryGetPersistBuildConfig distPref
  where
    explainError :: ConfigStateFileError -> a
    explainError x@ConfigStateFileBadVersion{} | useStack = stackExplanation x
    explainError x = error ("Error: " ++ show x)
    stackExplanation x = error ("Error: " ++ show x ++ "\n\nYou can probably fix this by running:\n  stack setup --upgrade-cabal")
#else
getLbi _ distPref = getPersistBuildConfig distPref
#endif

main :: IO ()
main = do
    Opts {..} <- execParser $
                 info (helper <*> opts)
                      (header helpHeader <>
                       fullDesc <>
                       footerDoc (Just helpFooter))

    -- print opts'

    useStack <- findRecursive "stack.yaml"
    distPref <- chooseDistPref useStack

    lbiExists <- doesFileExist (localBuildInfoFile distPref)
    unless lbiExists $ do
        putStrLn "*ERROR* package not properly configured yet or not in top-level CABAL project folder; see --help for more details"
        exitFailure

    lbiMTime <- getModificationTime (localBuildInfoFile distPref)

    lbi <- getLbi useStack distPref

    -- minory sanity checking
    case pkgDescrFile lbi of
        Nothing -> fail "could not find .cabal file"
        Just pkgDescFile -> do
            res <- checkPersistBuildConfigOutdated distPref pkgDescFile
            when res $ putStrLn "*WARNING* outdated config-data -- please re-configure"

    let cbo = compBuildOrder lbi
        pkg = localPkgDescr lbi
        ipkgs = installedPkgs lbi

    importsInOutDir <- case compilerId (compiler lbi) of
        CompilerId GHC (Version v _) | v >= [7,8] -> return True
        CompilerId GHC _ -> return False
        CompilerId _ _ -> putStrLn "*WARNING* non-GHC compiler detected" >> return False

    putHeading "detected package components"

    when (isJust $ PD.library pkg) $
        putStrLn $ " - library" ++ [ '*' | CLibName `notElem` cbo ]
    unless (null $ PD.executables pkg) $
        putStrLn $ " - executable(s): " ++ unwords [ n ++ [ '*' | CExeName n `notElem` cbo ]
                                                   | PD.Executable { exeName = n } <- PD.executables pkg ]
    unless (null $ PD.testSuites pkg) $
        putStrLn $ " - testsuite(s): " ++ unwords [ n ++ [ '*' | CTestName n `notElem` cbo ]
                                                  | PD.TestSuite { testName = n } <- PD.testSuites pkg ]
    unless (null $ PD.benchmarks pkg) $
        putStrLn $ " - benchmark(s): " ++ unwords [ n ++ [ '*' | CBenchName n `notElem` cbo ]
                                                  | PD.Benchmark { benchmarkName = n} <- PD.benchmarks pkg ]

    putStrLn ""
    putStrLn "(component names suffixed with '*' are not configured to be built)"
    putStrLn ""

    ----------------------------------------------------------------------------

    -- GHC prior to 7.8.1 emitted .imports file in $PWD and therefore would risk overwriting files
    let multiMainIssue = not importsInOutDir && length (filter (/= CLibName) cbo) > 1


    ok <- newIORef True

    -- handle stanzas
    withAllComponentsInBuildOrder pkg lbi $ \c clbi -> do
        let (n,n2,cmods) = componentNameAndModules (not ignoreMainModule) c
            outDir = if null n2 then buildDir lbi else buildDir lbi </> n2 </> n2++"-tmp"
            outDir' = if importsInOutDir then outDir else  "."

        -- import dependancy graph read in via '.imports' files
        mods <- mapM (readImports outDir') =<< findImportsFiles outDir' lbiMTime

        -- imported modules by component
        let allmods | ignoreEmptyImports  = nub [ m | (mn, imps) <- mods
                                                    , mn `elem` cmods
                                                    , (m,_:_) <- imps
                                                    ]
                    | otherwise           = nub [ m | (mn, imps) <- mods
                                                    , mn `elem` cmods
                                                    , (m,_) <- imps
                                                    ]

            pkgs = componentPackageDeps clbi

            ipinfos = [ fromMaybe (error (show ipkgid)) $ lookupInstalledPackageId ipkgs ipkgid
                      | (ipkgid@(InstalledPackageId i), _) <- pkgs
                      , not ("-inplace" `isSuffixOf` i)
                      ]

            (ignored, unignored) = partition (\x -> display (pkgName $ packageId x) `elem` ignoredPackages) ipinfos

            unused = [ installedPackageId ipinfo
                     | ipinfo <- unignored
#if MIN_VERSION_Cabal(1,21,0)
                     , let expmods = map exposedName $ exposedModules ipinfo
#else
                     , let expmods = exposedModules ipinfo
#endif
                     , not (any (`elem` allmods) expmods)
                     ]

            missingMods = cmods \\ map fst mods

        -- print out redundant package ids (if any)
        putHeading n

        unless (null missingMods) $ do
            putStrLn "*WARNING* dependency information for the following component module(s) is missing: "
            forM_ missingMods $ \m -> putStrLn $ " - " ++ display m
            putStrLn ""

        when (not ignoreMainModule && multiMainIssue && not (compIsLib c)) $ do
            putStrLn "*WARNING* multiple non-library components detected"
            putStrLn "  result may be unreliable if there are multiple non-library components because the 'Main.imports' file gets overwritten with GHC prior to version 7.8.1, try"
            putStrLn ""
            putStrLn $ "  rm "++(outDir </> "Main.h")++"; cabal build --ghc-option=-ddump-minimal-imports; packunused"
            putStrLn ""
            putStrLn "  to get a more accurate result for this component."
            putStrLn ""

        unless (null ignored) $ do
            let k = length ignored
            putStrLn $ "Ignoring " ++ show k ++ " package" ++ (if k == 1 then "" else "s")
            putStrLn ""

        if null unused
          then do
            putStrLn "no redundant packages dependencies found"
            putStrLn ""
          else do
            putStrLn "The following package dependencies seem redundant:"
            putStrLn ""
            forM_ unused $ \pkg' -> putStrLn $ " - " ++ display pkg'
            putStrLn ""
            writeIORef ok False

    whenM (not <$> readIORef ok) exitFailure
  where

    compIsLib CLib {} = True
    compIsLib _       = False

    findImportsFiles outDir lbiMTime = do
        whenM (not `fmap` doesDirectoryExist outDir) $
            fail $"output-dir " ++ show outDir ++ " does not exist; -- has 'cabal build' been performed yet? (see also 'packunused --help')"

        files <- filterM (doesFileExist . (outDir</>)) =<<
                 liftM (sort . filter (isSuffixOf ".imports"))
                 (getDirectoryContents outDir)

        when (null files) $
            fail $ "no .imports files found in " ++ show outDir ++ " -- has 'cabal build' been performed yet? (see also 'packunused --help')"

        -- .import files generated after lbi
        files' <- filterM (liftM (> lbiMTime) . getModificationTime . (outDir</>)) files

        unless (files' == files) $ do
            putStrLn "*WARNING* some possibly outdated .imports were found (please consider removing/rebuilding them):"
            forM_ (files \\ files') $ \fn -> putStrLn $ " - " ++ fn
            putStrLn ""

        -- when (null files') $
        --    fail "no up-to-date .imports files found -- please perform 'cabal build'"

        return files

#if MIN_VERSION_Cabal(1,18,0)
    compBuildOrder = map fst . allComponentsInBuildOrder
#else
    withAllComponentsInBuildOrder = withComponentsLBI
#endif

componentNameAndModules :: Bool -> Component -> (String, String, [ModuleName])
componentNameAndModules addMainMod c  = (n, n2, m)
  where
    m = nub $ sort $ m0 ++ PD.otherModules (componentBuildInfo c)

    (n, n2, m0) = case c of
        CLib   ci -> ("library", "", PD.exposedModules ci)
        CExe   ci -> ("executable("++PD.exeName ci++")", PD.exeName ci, [mainModName | addMainMod ])
        CBench ci -> ("benchmark("++PD.benchmarkName ci++")", PD.benchmarkName ci, [mainModName | addMainMod ])
        CTest  ci -> ("testsuite("++PD.testName ci++")", PD.testName ci, [mainModName | addMainMod ])

    mainModName = MN.fromString "Main"

#if !MIN_VERSION_Cabal(1,16,0)
    componentBuildInfo = foldComponent PD.libBuildInfo PD.buildInfo PD.testBuildInfo PD.benchmarkBuildInfo
#endif

putHeading :: String -> IO ()
putHeading s = do
    putStrLn s
    putStrLn (replicate (length s) '~')
    putStrLn ""

-- empty symbol list means '()'
readImports :: FilePath -> FilePath -> IO (ModuleName, [(ModuleName, [String])])
readImports outDir fn = do
    unless (".imports" `isSuffixOf` fn) $
        fail ("argument "++show fn++" doesn't have .imports extension")

    let m = MN.fromString $ take (length fn - length ".imports") fn

    contents <- readFile (outDir </> fn)
    case parseImportsFile contents of
        (H.ParseOk (H.Module _ _ _ _ _ imps _)) -> do
            let imps' = [ (MN.fromString mn, extractSpecs (H.importSpecs imp))
                        | imp <- imps, let H.ModuleName mn = H.importModule imp ]

            return (m, imps')
        H.ParseFailed loc msg -> do
            putStrLn "*ERROR* failed to parse .imports file"
            putStrLn $ H.prettyPrint loc ++ ": " ++ msg
            exitFailure

  where
    extractSpecs (Just (False, impspecs)) = map H.prettyPrint impspecs
    extractSpecs _ = error "unexpected import specs"

    parseImportsFile = H.parseFileContentsWithMode (H.defaultParseMode { H.extensions = exts, H.parseFilename = outDir </> fn }) . stripExplicitNamespaces . stripSafe

    -- hack to remove -XExplicitNamespaces until haskell-src-exts supports that
    stripExplicitNamespaces = unwords . splitOn " type "
    stripSafe = unwords . splitOn " safe "

#if MIN_VERSION_haskell_src_exts(1,14,0)
    exts = map H.EnableExtension [ H.MagicHash, H.PackageImports, H.CPP, H.TypeOperators, H.TypeFamilies {- , H.ExplicitNamespaces -} ]
#else
    exts = [ H.MagicHash, H.PackageImports, H.CPP, H.TypeOperators, H.TypeFamilies ]
#endif

-- | Find if a file exists in the current directory or any of its
-- parents.
findRecursive :: FilePath -> IO Bool
findRecursive f = do
  dir <- getCurrentDirectory
  go dir
  where
    go dir = do
      exists <- doesFileExist (dir </> f)
      if exists
      then return exists
      else
        let parent = takeDirectory dir
        in if parent == dir
           then return False
           else go parent

whenM :: Monad m => m Bool -> m () -> m ()
whenM test = (test >>=) . flip when
