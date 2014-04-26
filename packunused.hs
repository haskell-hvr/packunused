{-# LANGUAGE CPP, DeriveDataTypeable, RecordWildCards #-}

module Main where

import           Control.Monad
import           Data.IORef
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Maybe
import           Data.Version (Version(Version), showVersion)
import           Distribution.InstalledPackageInfo (exposedModules, installedPackageId)
import           Distribution.ModuleName (ModuleName)
import           Distribution.Simple.Compiler
import qualified Distribution.ModuleName as MN
import           Distribution.Package (InstalledPackageId(..), packageId, pkgName)
import qualified Distribution.PackageDescription as PD
import           Distribution.Simple.Configure (localBuildInfoFile, getPersistBuildConfig, checkPersistBuildConfigOutdated)
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PackageIndex (lookupInstalledPackageId)
import           Distribution.Simple.Utils (cabalVersion)
import           Distribution.Text (display)
import qualified Language.Haskell.Exts as H
import           System.Console.CmdArgs.Implicit
import           System.Directory (getModificationTime, getDirectoryContents, doesDirectoryExist, doesFileExist)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))

import           Paths_packunused (version)

-- |CLI Options
data Opts = Opts
    { ignoreEmptyImports :: Bool
    , ignoreMainModule :: Bool
    , ignoredPackages :: [String]
    } deriving (Show, Data, Typeable)

opts :: Opts
opts = Opts
    { ignoreEmptyImports = def &= name "ignore-empty-imports" &= explicit
    , ignoreMainModule   = def &= name "ignore-main-module" &= explicit
    , ignoredPackages    = def &= name "ignore-package" &= explicit &= typ "PACKAGE" &= help "ignore the specfied package in the report"
    }
    &= program "packunused"
    &= summary ("packunused " ++ showVersion version ++ " (using Cabal "++ showVersion cabalVersion ++ ")")
    &= details helpDesc

helpDesc :: [String]
helpDesc = [ "Tool to help find redundant build-dependencies in CABAL projects"
           , ""
           , "In order to use this package you should set up the package as follows, before executing 'packunused':"
           , ""
           , " cabal clean"
           , " rm *.imports      # (only needed for GHC<7.8)"
           , " cabal configure -O0 --disable-library-profiling"
           , " cabal build --ghc-option=-ddump-minimal-imports"
           , " packunused"
           , ""
           , "Note: The 'cabal configure' command above only tests the default package configuration." ++
             "You might need to repeat the process with different flags added to the 'cabal configure' step " ++
             "(such as '--enable-tests' or '--enable-benchmark' or custom cabal flags) " ++
             "to make sure to check all configurations"
           ]

main :: IO ()
main = do
    Opts {..} <- cmdArgs opts

    -- print opts'

    lbiExists <- doesFileExist (localBuildInfoFile distPref)
    unless lbiExists $ do
        putStrLn "*ERROR* package not properly configured yet or not in top-level CABAL project folder; see --help for more details"
        exitFailure

    lbiMTime <- getModificationTime (localBuildInfoFile distPref)
    lbi <- getPersistBuildConfig distPref


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
                     , let expmods = exposedModules ipinfo
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

    aok <- readIORef ok
    unless aok exitFailure
  where
    distPref = "./dist"

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


whenM :: Monad m => m Bool -> m () -> m ()
whenM test = (test >>=) . flip when
