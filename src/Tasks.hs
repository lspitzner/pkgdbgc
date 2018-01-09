{-# LANGUAGE TypeApplications #-}
module Tasks where



import qualified System.Exit
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.List                     as List
import qualified Data.Foldable
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import qualified Data.Graph.Inductive          as G
import qualified Data.Maybe                    as Maybe
import           Data.Ord
import qualified System.IO
import           Control.Exception
import           Text.Read                      ( readMaybe )
import qualified System.Directory              as Directory
import           System.FilePath
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Text               as Aeson
import qualified Data.Text.Lazy                as TextL
import qualified System.Process

import           Distribution.InstalledPackageInfo
import           Distribution.Text              ( disp )
import qualified Distribution.Types.UnitId     as Cabal

import           Text.Printf

import           Data.Bifunctor

import qualified Cabal.Plan

import           Utils
import           DbParser
import           RootRegistry



statsTask :: [FilePath] -> IO ()
statsTask dbPathL = do
  infos <- getPkgInfosOrExit dbPathL
  let infoCount = length infos
  putStrTable "number of db entries" (show infoCount)
  let nameMap = Map.fromListWith
        (++)
        [ (sourcePackageId info, [info]) | info <- infos ]
  let pkgVersionCount = Map.size nameMap
  putStrTable "number of distinct (pkg, version) tuples" (show pkgVersionCount)
  putStrTable
    "=> avg. number of same-package installs"
    ( printf
      "%.2f"
      ((fromIntegral infoCount :: Float) / fromIntegral pkgVersionCount)
    )
  putStrTable
    "package with most duplicates"
    ( let (pv, is) =
            List.maximumBy (comparing (length . snd)) (Map.toList nameMap)
      in  show (disp pv) ++ ", " ++ show (length is) ++ " entries"
    )
  let pkgGraph :: G.Gr InstalledPackageInfo () = fst $ createPkgGraph infos
  putStrTable "graph component count" (show $ length $ G.bcc pkgGraph)
  putStrTable
    "graph node count outdegree=0"
    (show $ length [ n | n <- G.nodes pkgGraph, G.outdeg pkgGraph n == 0 ])
  let zeroIns = [ n | n <- G.nodes pkgGraph, G.indeg pkgGraph n == 0 ]
  putStrTable "graph node count indegree=0" (show $ length zeroIns)
  putStrTable "biggest (package,version)s with indegree=0" ""
  do
    let sizeMap = Map.fromListWith
          Set.union
          [ ( case G.lab pkgGraph root of
              Nothing   -> undefined
              Just info -> sourcePackageId info
            , Set.fromList $ G.bfs root pkgGraph
            )
          | root <- zeroIns
          ]
    mapM_
      (putStrTable "")
      ( take 5
      $ fmap (\(a, b) -> show (disp a) ++ ", " ++ show (b - 1) ++ " deps")
      $ List.sortOn (Down . snd)
      $ fmap (second Set.size)
      $ Map.toList sizeMap
      )
  pure () -- TODO


dbListUnitsTask :: [FilePath] -> IO ()
dbListUnitsTask dbPathL = do
  infos <- getPkgInfosOrExit dbPathL
  let pkgGraph :: G.Gr InstalledPackageInfo () = fst $ createPkgGraph infos
  mapM_ putStrLn
        (map (show . disp . installedUnitId . snd) $ G.labNodes pkgGraph)


dbGraphTask :: [FilePath] -> IO ()
dbGraphTask dbPathL = do
  infos <- getPkgInfosOrExit dbPathL
  let pkgGraph :: G.Gr InstalledPackageInfo () = fst $ createPkgGraph infos
  G.prettyPrint (first (show . disp . sourcePackageId) pkgGraph)


planInfoTask :: [FilePath] -> IO ()
planInfoTask plansFileL = do
  plans <- getPlansFromFiles plansFileL
  putStrLn $ printf
    "got %d plans referencing %d unit ids"
    (length plans)
    (length $ plans >>= (snd .> Cabal.Plan.pjUnits .> Map.keys))


planListTask :: [FilePath] -> IO ()
planListTask plansFileL = do
  plans <- getPlansFromFiles plansFileL
  mapM_
    putStrLn
    [ show $ disp $ id @Cabal.UnitId $ read $ show unit
    | (_, plan) <- plans
    , unit      <- Map.keys (Cabal.Plan.pjUnits plan)
    ]


planRootsTask :: [FilePath] -> [FilePath] -> IO ()
planRootsTask dbPathL plansFileL = do
  infos <- getPkgInfosOrExit dbPathL
  plans <- getPlansFromFiles plansFileL
  let planUnits = extractPlansUnits $ map snd plans
  let (pkgGraph :: G.Gr InstalledPackageInfo (), unitMap) =
        createPkgGraph infos
  let planNodes =
        [ n | unit <- planUnits, Just n <- [Map.lookup unit unitMap] ]
  let zeroIns = [ n | n <- planNodes, G.indeg pkgGraph n == 0 ]
  mapM_
    putStrLn
    [ case G.lab pkgGraph n of
        Nothing   -> undefined
        Just info -> show $ disp $ installedUnitId info
    | n <- zeroIns
    ]


planCoverageTask :: [FilePath] -> [FilePath] -> IO ()
planCoverageTask dbPathL plansFileL = do
  infos <- getPkgInfosOrExit dbPathL
  plans <- getPlansFromFiles plansFileL
  let units = extractPlansUnits $ map snd plans
  let (pkgGraph :: G.Gr InstalledPackageInfo (), unitMap) =
        createPkgGraph infos
  let planNodes = [ n | unit <- units, Just n <- [Map.lookup unit unitMap] ]
  let infoCount = length infos
  putStrTable "number of db entries"            (show infoCount)
  putStrTable "unit count in plans"             (show $ length units)
  putStrTable "unit count in plans found in db" (show $ length planNodes)
  putStrTable "transitively reachable in db"
              (show $ length $ G.dfs planNodes pkgGraph)
  pure ()


rootsCoverageTask :: [FilePath] -> [FilePath] -> IO ()
rootsCoverageTask dbPathL rootsFileL = do
  infos <- getPkgInfosOrExit dbPathL
  roots <- getRootsOrExit rootsFileL
  -- let units = extractPlansUnits plans
  let (pkgGraph :: G.Gr InstalledPackageInfo (), unitMap) =
        createPkgGraph infos
  let rootNodes = [ n | unit <- roots, Just n <- [Map.lookup unit unitMap] ]
  let infoCount = length infos
  putStrTable "number of db entries"            (show infoCount)
  putStrTable "root count from file"            (show $ length roots)
  putStrTable "root count in plans found in db" (show $ length rootNodes)
  putStrTable "transitively reachable in db"
              (show $ length $ G.dfs rootNodes pkgGraph)
  pure ()


registryCoverageTask :: [FilePath] -> IO ()
registryCoverageTask dbPathL = do
  infos <- getPkgInfosOrExit dbPathL
  roots <- getRegistryRoots >>= \case
    Left err -> do
      putStrErrLn err
      System.Exit.exitFailure
    Right x -> pure x
  -- let units = extractPlansUnits plans
  let (pkgGraph :: G.Gr InstalledPackageInfo (), unitMap) =
        createPkgGraph infos
  let rootNodes = [ n | unit <- roots, Just n <- [Map.lookup unit unitMap] ]
  let infoCount = length infos
  putStrTable "number of db entries"            (show infoCount)
  putStrTable "root count from file"            (show $ length roots)
  putStrTable "root count in plans found in db" (show $ length rootNodes)
  putStrTable "transitively reachable in db"
              (show $ length $ G.dfs rootNodes pkgGraph)
  pure ()


registerTask :: Maybe FilePath -> IO ()
registerTask distNewstylePathM = do
  planFilePath <- case distNewstylePathM of
    Nothing -> do
      cwd <- liftIO $ Directory.getCurrentDirectory
      pure $ cwd </> "dist-newstyle" </> "cache" </> "plan.json"
    Just x -> pure $ x </> "cache" </> "plan.json"
  exists <- liftIO $ Directory.doesFileExist planFilePath
  unless exists $ do
    putStrErrLn $ "could not find plan.json"
    System.Exit.exitFailure
  planJson <- getPlanFromFileOrExit planFilePath
  let compilerString = read $ TextL.unpack $ Aeson.encodeToLazyText
        (Cabal.Plan.pjCompilerId planJson)
  putStrErrLn $ "compiler: " ++ compilerString
  e <- registerHelper [(planFilePath, planJson)]
  case e of
    Left err -> do
      putStrErrLn err
      System.Exit.exitFailure
    Right () -> pure ()
  putStrErrLn "successfully added to registry"

reRegister :: [FilePath] -> IO (Either String ())
reRegister planFilePaths = do
  planJsons <- fmap join $ planFilePaths `forM` \planFilePath -> do
    try (Cabal.Plan.decodePlanJson planFilePath) >>= \case
      Left (err :: SomeException) -> do
        putStrErrLn
          $  "warning: could not parse "
          ++ planFilePath
          ++ ": "
          ++ show err
        pure []
      Right x -> pure [(planFilePath, x)]
  registerHelper planJsons

registerHelper :: [(FilePath, Cabal.Plan.PlanJson)] -> IO (Either String ())
registerHelper planJsons = do
  tuples <- planJsons `forM` \(planFilePath, planJson) -> do
    let key = RootKey
          { _rk_location = planFilePath
          , _rk_compiler = Cabal.Plan.pjCompilerId planJson
          , _rk_reason   = Build
          }
    let planUnits = extractPlansUnits [planJson]
    let uids      = UnitId <$> planUnits
    pure (key, uids)
  addRegistryRoots tuples


registryRecheckPlansTask :: Bool -> IO ()
registryRecheckPlansTask verbose = do
  paths <- getRegistryPlanPaths >>= \case
    Left err -> do
      putStrErrLn err
      System.Exit.exitFailure
    Right x -> pure x
  when verbose $ do
    putStrErrLn "Re-registering roots from plans:"
    paths `forM_` \p -> putStrErrLn $ "  " ++ p
  e <- reRegister $ Set.toList paths
  case e of
    Left err -> do
      putStrErrLn err
      System.Exit.exitFailure
    Right () -> pure ()


storeGcTask
  :: [FilePath] -> [String] -> [String] -> [String] -> Bool -> Bool -> IO ()
storeGcTask storepathL ghcpkgL compilerL dbPathL dry verbose = do
  ghcpkg <- case ghcpkgL of
    [x] -> pure x
    []  -> pure $ "ghc-pkg"
    _   -> do
      putStrErrLn $ "Must specify --with-ghc-pkg at most once"
      System.Exit.exitFailure
  compiler <- case compilerL of
    [] -> do
      ghcpkgVersionString <- System.Process.readProcess ghcpkg ["--version"] ""
      case words ghcpkgVersionString of
        ["GHC", "package", "manager", "version", v] -> do
          let r = "ghc-" ++ v
          putStrTable "inferred ghc version" r
          pure r
        _ -> do
          putStrErrLn $ "could not determine version of ghc-pkg"
          System.Exit.exitFailure
    [x] -> pure x
    _   -> do
      putStrErrLn
        $  "Must specify exactly one specific compiler(s)"
        ++ " for which to run gc"
      System.Exit.exitFailure
  storepath <- case storepathL of
    [] -> do
      home <- Directory.getHomeDirectory
      pure $ home </> ".cabal" </> "store"
    [p] -> pure p
    _   -> do
      putStrErrLn "More than one store path, aborting"
      System.Exit.exitFailure
  pkgdir <- case dbPathL of
    [] -> do
      pure $ storepath </> compiler </> "package.db"
    -- [x]
    --   | null storepathL -> pure x
    --   | otherwise -> do
    --     putStrErrLn "Cannot specify both --package-db and --store-path"
    --     System.Exit.exitFailure
    _ -> do
      putStrErrLn "Must not specify --package-db for store-gc (will be inferred from --store-path)"
      System.Exit.exitFailure
  putStrLn "Refreshing roots from registered plans"
  registryRecheckPlansTask verbose
  putStrTable "package-db directory" pkgdir
  exists <- Directory.doesDirectoryExist pkgdir
  unless exists $ do
    putStrErrLn $ "package-db location not found: " ++ show pkgdir
    System.Exit.exitFailure
  infos <- getPkgInfosOrExit [pkgdir]
  roots <- getRegistryRoots >>= \case
    Left err -> do
      putStrErrLn err
      System.Exit.exitFailure
    Right x -> pure x
  -- let units = extractPlansUnits plans
  let (pkgGraph :: G.Gr InstalledPackageInfo (), unitMap) =
        createPkgGraph infos
  let rootNodes  = [ n | unit <- roots, Just n <- [Map.lookup unit unitMap] ]
  let _infoCount = length infos
  putStrTable ("count of db entries for compiler " ++ compiler)
              (show $ G.order pkgGraph)
  putStrTable "count of entries reachable from roots"
              (show $ length $ G.dfs rootNodes pkgGraph)
  let restGraph = G.delNodes rootNodes pkgGraph
  let toBeDropped = G.topsort' restGraph
  when (G.order restGraph /= length toBeDropped) $ do
    putStrErrLn "internal error: restGraph size /= toBeDropped length"
    System.Exit.exitFailure
  putStrTable "count of entries to be dropped" (show $ length toBeDropped)
  if dry
    then do
      putStrErrLn "Stopping due to dry-run"
      when verbose $ do
        putStrLn "Would drop:"
        toBeDropped `forM_` \x -> do
          putStrLn $ "  " ++ (show $ disp $ installedUnitId $ x)
    else do
      toBeDropped `forM_` \uid -> do
        let uidStr = show $ disp $ installedUnitId uid
        let args   = ["--package-db", pkgdir, "unregister", "--ipid", uidStr]
        when verbose $ do
          putStrErrLn $ "executing `" ++ ghcpkg ++ " " ++ unwords args ++ "`"
        (exitCode, stdout, stderr) <- System.Process.readProcessWithExitCode
          ghcpkg
          args
          ""
        case exitCode of
          System.Exit.ExitSuccess   -> pure ()
          System.Exit.ExitFailure i -> do
            putStrErrLn
              $  "ghc-pkg unregister failed ("
              ++ show i
              ++ ") for "
              ++ uidStr
            putStr stdout
            putStr stderr
            System.Exit.exitFailure
        let packageDirToDelete = storepath </> compiler </> uidStr
        when verbose $ do
          putStrErrLn $ "deleting directory `" ++ packageDirToDelete ++ "`"
        Directory.removeDirectoryRecursive packageDirToDelete

