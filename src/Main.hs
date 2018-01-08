{-# LANGUAGE TypeApplications #-}
module Main where



import           Data.Functor.Identity
import qualified System.Exit
import           Control.Monad
import           Data.Version
import           Control.Monad.IO.Class
import qualified Data.List                     as List
import qualified Data.Foldable
import qualified System.IO
import           Control.Exception

import           UI.Butcher.Monadic
import           UI.Butcher.Monadic.Types

import           Paths_pkgdbgc
import           Tasks



main :: IO ()
main = mainFromCmdParserWithHelpDesc mainCmdParser

-- fixed version until new butcher release
addHelpCommand' :: Applicative f => CommandDesc a -> CmdParser f (IO ()) ()
addHelpCommand' desc = addCmd "help" $ do
  rest <- addParamRestOfInput "SUBCOMMAND(s)" mempty
  addCmdImpl $ do
    let restWords = List.words rest
    let
      descent :: [String] -> CommandDesc a -> CommandDesc a
      descent [] curDesc = curDesc
      descent (w:wr) curDesc =
        case
            List.lookup (Just w) $ Data.Foldable.toList $ _cmd_children curDesc
          of
            Nothing    -> curDesc
            Just child -> descent wr child
    print $ ppHelpShallow $ descent restWords desc

mainCmdParser :: CommandDesc () -> CmdParser Identity (IO ()) ()
mainCmdParser helpDesc = do
  addCmdSynopsis "garbage collection for ghc-pkg databases"
  -- addCmdHelp $ helpDoc
  reorderStart
  _printHelp   <- addSimpleBoolFlag "h" ["help"] mempty
  printVersion <- addSimpleBoolFlag "" ["version"] mempty
  dbPathL      <- addFlagStringParams [] ["package-db"] "DBPATH" mempty
  plansFileL   <- addFlagStringParams [] ["plans-file"] "PLANSFILE" mempty
  rootsFileL   <- addFlagStringParams [] ["roots-file"] "ROOTSFILE" mempty
  compilerL    <- addFlagStringParams [] ["compiler"] "COMPILER" mempty
  storepathL   <- addFlagStringParams [] ["store-path"] "STOREPATH" mempty
  ghcpkgL      <- addFlagStringParams [] ["with-ghc-pkg"] "GHCPKG" mempty
  dryGlob      <- addSimpleBoolFlag [] ["dryrun", "dry-run"] mempty
  verboseGlob  <- addSimpleBoolFlag "v" ["verbose"] mempty
  reorderStop
  addHelpCommand' helpDesc
  addCmdImpl $ void $ do
    when printVersion $ do
      do
        putStrLn $ "pkgdbgc version " ++ showVersion version
        putStrLn $ "Copyright (C) 2016-2017 Lennart Spitzner"
        putStrLn $ "There is NO WARRANTY, to the extent permitted by law."
      System.Exit.exitSuccess
    -- when printHelp $ do
    liftIO $ print $ ppHelpShallow helpDesc
    System.Exit.exitSuccess
  addCmd "db-stats" $ addCmdImpl $ statsTask dbPathL
  addCmd "db-list-units" $ addCmdImpl $ dbListUnitsTask dbPathL
  addCmd "db-graph" $ addCmdImpl $ dbGraphTask dbPathL
  addCmd "plan-info" $ addCmdImpl $ planInfoTask plansFileL
  addCmd "plan-list" $ addCmdImpl $ planListTask plansFileL
  addCmd "plan-roots" $ addCmdImpl $ planRootsTask dbPathL plansFileL
  addCmd "plan-coverage" $ addCmdImpl $ planCoverageTask dbPathL plansFileL
  addCmd "roots-coverage" $ addCmdImpl $ rootsCoverageTask dbPathL rootsFileL
  addCmd "registry-coverage" $ addCmdImpl $ registryCoverageTask dbPathL
  addCmd "register-dist-newstyle" $ do
    distNewstylePathM <- addParamStringOpt "DISTNEWPATH" mempty
    addCmdImpl $ registerTask distNewstylePathM
  addCmd "store-gc" $ do
    reorderStart
    dry     <- addSimpleBoolFlag [] ["dryrun", "dry-run"] mempty
    verbose <- addSimpleBoolFlag "v" ["verbose"] mempty
    reorderStop
    addCmdImpl $ storeGcTask storepathL
                             ghcpkgL
                             compilerL
                             dbPathL
                             (dryGlob || dry)
                             (verboseGlob || verbose)

