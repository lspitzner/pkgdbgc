module Utils where



import qualified System.Exit
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import qualified Data.Graph.Inductive          as G
import qualified System.IO

import           Control.Exception
import           Control.Monad
import           Text.Read                      ( readMaybe )

-- Cabal and cabal-plan
import qualified Distribution.Types.UnitId     as Cabal
import           Distribution.InstalledPackageInfo
                                                ( InstalledPackageInfo
                                                , depends
                                                , installedUnitId
                                                )
import qualified Cabal.Plan

-- internal modules
import           DbParser



putStrTable :: String -> String -> IO ()
putStrTable a b =
  putStrLn (a ++ ":" ++ replicate (max 1 (51 - length a)) ' ' ++ b)

getPkgInfosOrExit :: [FilePath] -> IO [InstalledPackageInfo]
getPkgInfosOrExit [dbPath] = do
  putStrErr "parsing package db.. "
  infosE <- getPkgInfos dbPath
  case infosE of
    Left str -> do
      putStrErrLn str
      System.Exit.exitFailure
    Right x -> do
      putStrErrLn $ "success"
      pure x
getPkgInfosOrExit _ = do
  putStrErrLn "Requires exactly one --package-db to inspect"
  System.Exit.exitFailure

getPlansFromFiles :: [FilePath] -> IO [(FilePath, Cabal.Plan.PlanJson)]
getPlansFromFiles planFiles = do
  planDirs <- fmap (>>= lines) $ mapM readFile planFiles
  fmap join $ planDirs `forM` \plan -> do
    try (Cabal.Plan.decodePlanJson plan) >>= \case
      Left (err :: SomeException) -> do
        putStrErrLn $ "warning: could not parse " ++ plan ++ ": " ++ show err
        pure []
      Right x -> pure [(plan, x)]

getPlanFromFileOrExit :: FilePath -> IO Cabal.Plan.PlanJson
getPlanFromFileOrExit planFile = do
  try (Cabal.Plan.decodePlanJson planFile) >>= \case
    Left (err :: SomeException) -> do
      putStrErrLn $ "error: could not parse " ++ planFile ++ ": " ++ show err
      System.Exit.exitFailure
    Right x -> pure x


getRootsOrExit :: [FilePath] -> IO [Cabal.UnitId]
getRootsOrExit rootsFiles = do
  rootStrs <- fmap (>>= lines) $ mapM readFile rootsFiles
  fmap join $ rootStrs `forM` \str ->
    case readMaybe ("UnitId \"" ++ str ++ "\"") of
      Nothing -> do
        putStrErr $ "warning: could not parse root " ++ show str
        pure []
      Just x -> pure [x]

extractPlansUnits :: [Cabal.Plan.PlanJson] -> [Cabal.UnitId]
extractPlansUnits plans = Set.toList $ Set.fromList
  [ read (show unit)
  | plan <- plans
  , unit <- Map.keys (Cabal.Plan.pjUnits plan)
  ]

createPkgGraph
  :: G.Graph gr
  => [InstalledPackageInfo]
  -> (gr InstalledPackageInfo (), Map.Map Cabal.UnitId G.Node)
createPkgGraph infos =
  ( G.mkGraph
    iinfos
    [ (i, j, ())
    | (i, info) <- iinfos
    , Just j    <- (\uid -> Map.lookup uid unitMap) <$> depends info
    ]
  , unitMap
  )
 where
  iinfos  = zip [0 ..] infos
  unitMap = Map.fromList ((\(i, info) -> (installedUnitId info, i)) <$> iinfos)


putStrErr :: String -> IO ()
putStrErr s = System.IO.hPutStr System.IO.stderr s

putStrErrLn :: String -> IO ()
putStrErrLn s = System.IO.hPutStrLn System.IO.stderr s

printErr :: Show a => a -> IO ()
printErr = putStrErrLn . show

