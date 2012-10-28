{-# LANGUAGE CPP, DeriveDataTypeable, RecordWildCards #-}

module Main where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Version (showVersion)
import           Distribution.InstalledPackageInfo (exposedModules, installedPackageId)
import           Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as MN
import           Distribution.Package (InstalledPackageId(..))
import qualified Distribution.PackageDescription as PD
import           Distribution.Simple.Configure (localBuildInfoFile, getPersistBuildConfig, checkPersistBuildConfigOutdated)
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PackageIndex (lookupInstalledPackageId)
import           Distribution.Simple.Utils (cabalVersion)
import           Distribution.Text (display)
import qualified Language.Haskell.Exts as H
import           System.Console.CmdArgs.Implicit
import           System.Directory (getModificationTime, getDirectoryContents, doesFileExist)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))

import           Paths_packunused (version)

#if !MIN_VERSION_Cabal(1,16,0)
componentBuildInfo :: Component -> PD.BuildInfo
componentBuildInfo = foldComponent PD.libBuildInfo PD.buildInfo PD.testBuildInfo PD.benchmarkBuildInfo
#endif

-- |CLI Options
data Opts = Opts
    { ignoreEmptyImports :: Bool
    , ignoreMainModule :: Bool
    } deriving (Show, Data, Typeable)

opts :: Opts
opts = Opts
    { ignoreEmptyImports = def &= explicit &= name "ignore-empty-imports"
    , ignoreMainModule   = def &= explicit &= name "ignore-main-module"
    }
    &= program "packunused"
    &= summary ("packunused " ++ showVersion version ++ " (using Cabal "++ showVersion cabalVersion ++ ")")
    &= details helpDesc

helpDesc :: [String]
helpDesc = [ "Tool to help find redundant build-dependancies in CABAL projects"
           , ""
           , "In order to use this package you should set up the package as follows, before executing 'packunused':"
           , ""
           , " cabal clean"
           , " rm *.imports"
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

    let cbo = compBuildOrder lbi

    -- minory sanity checking
    case pkgDescrFile lbi of
        Nothing -> fail "could not find .cabal file"
        Just pkgDescFile -> do
            res <- checkPersistBuildConfigOutdated distPref pkgDescFile
            when res $ putStrLn "*WARNING* outdated config-data -- please re-configure"

    putHeading "detected package components"

    when (isJust $ libraryConfig lbi) $
        putStrLn $ " - library" ++ [ '*' | CLibName `notElem` cbo ]
    unless (null $ executableConfigs lbi) $
        putStrLn $ " - executable(s): " ++ unwords [ n ++ [ '*' | CExeName n `notElem` cbo ]
                                                   | (n,_) <- executableConfigs lbi ]
    unless (null $ testSuiteConfigs lbi) $
        putStrLn $ " - testsuite(s): " ++ unwords [ n ++ [ '*' | CTestName n `notElem` cbo ]
                                                  | (n,_) <- testSuiteConfigs lbi ]
    unless (null $ benchmarkConfigs lbi) $
        putStrLn $ " - benchmark(s): " ++ unwords [ n ++ [ '*' | CBenchName n `notElem` cbo ]
                                                  | (n,_) <- benchmarkConfigs lbi ]
    putStrLn ""
    putStrLn "(component names suffixed with '*' are not configured to be built)"
    putStrLn ""

    let ipkgs = installedPkgs lbi

    files <- filterM doesFileExist =<< liftM (sort . filter (isSuffixOf ".imports")) (getDirectoryContents ".")

    when (null files) $
        fail "no .imports files found -- has 'cabal build' been performed yet? (see also --help)"

    -- .import files generated after lbi
    files' <- filterM (liftM (> lbiMTime) . getModificationTime) files

    unless (files' == files) $ do
        putStrLn "*WARNING* some possibly outdated .imports were found (please consider removing/rebuilding them):"
        forM_ (files \\ files') $ \fn -> putStrLn $ " - " ++ fn
        putStrLn ""

    -- when (null files') $
    --    fail "no up-to-date .imports files found -- please perform 'cabal build'"

    -- import dependancy graph read in via '.imports' files
    mods <- mapM readImports files

    let multiMain = length cbo > libCount+1
        libCount = if CLibName `elem` cbo then 1 else 0

    -- handle stanzas
    withComponentsLBI (localPkgDescr lbi) lbi $ \c clbi -> do
        let (n,n2,cmods) = componentNameAndModules (not ignoreMainModule) c

            -- imported modules by component
            allmods | ignoreEmptyImports  = nub [ m | (mn, imps) <- mods
                                                    , mn `elem` cmods
                                                    , (m,_:_) <- imps
                                                    ]
                    | otherwise           = nub [ m | (mn, imps) <- mods
                                                    , mn `elem` cmods
                                                    , (m,_) <- imps
                                                    ]

            pkgs = componentPackageDeps clbi

            mainObj | null n2   = ""
                    | otherwise = buildDir lbi </> n2 </> n2++"-tmp" </> "Main.o"

            ipinfos = [ fromMaybe (error (show ipkgid)) $ lookupInstalledPackageId ipkgs ipkgid
                      | (ipkgid@(InstalledPackageId i), _) <- pkgs
                      , not ("-inplace" `isSuffixOf` i)
                      ]

            unused = [ installedPackageId ipinfo
                     | ipinfo <- ipinfos
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

        when (not ignoreMainModule && multiMain && not (compIsLib c)) $ do
            putStrLn "*WARNING* multiple non-library components detected"
            putStrLn "  result may be unreliable if there are multiple non-library components because the 'Main.imports' file gets overwritten. Try"
            putStrLn ""
            putStrLn $ "  rm "++mainObj++"; cabal build --ghc-option=-ddump-minimal-imports; packunused"
            putStrLn ""
            putStrLn "  to get a more accurate result for this component."
            putStrLn ""

        if null unused
          then do
            putStrLn "no redundant packages depencencies found"
            putStrLn ""
          else do
            putStrLn "The following package depencencies seem redundant:"
            putStrLn ""
            forM_ unused $ \pkg -> putStrLn $ " - " ++ display pkg
            putStrLn ""

    return ()
  where
    distPref = "./dist"

    compIsLib CLib {} = True
    compIsLib _       = False

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

putHeading :: String -> IO ()
putHeading s = do
    putStrLn s
    putStrLn (replicate (length s) '~')
    putStrLn ""

-- empty symbol list means '()'
readImports :: FilePath -> IO (ModuleName, [(ModuleName, [String])])
readImports fn = do
    unless (".imports" `isSuffixOf` fn) $
        fail ("argument "++show fn++" doesn't have .imports extension")

    let m = MN.fromString $ take (length fn - length ".imports") fn

    parseRes <- H.parseFileWithExts exts fn
    case parseRes of
        (H.ParseOk (H.Module _ _ _ _ _ imps _)) -> do
            let imps' = [ (MN.fromString mn, extractSpecs (H.importSpecs imp))
                        | imp <- imps, let H.ModuleName mn = H.importModule imp ]

            return (m, imps')
        H.ParseFailed {} -> do
            print parseRes
            exitFailure

  where
    extractSpecs (Just (False, impspecs)) = map H.prettyPrint impspecs
    extractSpecs _ = error "unexpected import specs"

    exts = map id [ H.MagicHash, H.PackageImports, H.CPP, H.TypeOperators, H.TypeFamilies {- , H.ExplicitNamespaces -} ]
