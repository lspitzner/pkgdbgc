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
import qualified Text.PrettyPrint              as PP
import qualified System.Process                as Process
import           Text.Read
import           Data.Maybe

import           UI.Butcher.Monadic
import           UI.Butcher.Monadic.Types

import           Paths_pkgdbgc
import           Tasks


main :: IO ()
main = mainFromCmdParserWithHelpDesc mainCmdParser


mainCmdParser :: CommandDesc () -> CmdParser Identity (IO ()) ()
mainCmdParser helpDesc = do
  addCmdSynopsis "garbage collection for ghc-pkg databases"
  addCmdHelp helpDoc
  addShellCompletionCommand' mainCmdParser
  -- addCmdHelp $ helpDoc
  reorderStart
  -- we print help on empty input too, so this flag is just so it parses too.
  _printHelp   <- addSimpleBoolFlag "h" ["help"] mempty
  printVersion <- addSimpleBoolFlag "" ["version"] mempty
  dbPathL      <- addFlagStringParams [] ["package-db"] "DBPATH" flagHidden
  plansFileL   <- addFlagStringParams [] ["plans-file"] "PLANSFILE" flagHidden
  rootsFileL   <- addFlagStringParams [] ["roots-file"] "ROOTSFILE" flagHidden
  compilerL    <- addFlagStringParams
    []
    ["compiler"]
    "COMPILER"
    (  flagHelpStr
    $  "determines which of the package-dbs in the store"
    ++ " to affect. Will by default be derived from the ghc-pkg"
    ++ " version."
    )
  storepathL <- addFlagStringParams [] ["store-path"] "STOREPATH" mempty
  ghcpkgL    <- addFlagStringParams
    []
    ["with-ghc-pkg"]
    "GHCPKG"
    (  flagHelpStr
    $  "path of ghc-pkg command to be used;"
    ++ " this will also determine which package-db"
    ++ " in the store will be affected."
    )
  dryGlobal     <- addSimpleBoolFlag [] ["dryrun", "dry-run"] mempty
  verboseGlobal <- addSimpleBoolFlag "v" ["verbose"] mempty
  reorderStop
  addHelpCommand' helpDesc
  addCmdImpl $ void $ do
    when printVersion $ do
      versionTask
      System.Exit.exitSuccess
    colsM <- getTermCols
    putStrLn
      $ PP.renderStyle PP.style { PP.ribbonsPerLine = 1.0
                                , PP.lineLength     = fromMaybe 80 colsM
                                }
      $ ppHelpDepthOne helpDesc
    System.Exit.exitSuccess
  addCmdHidden "db-stats" $ addCmdImpl $ statsTask dbPathL
  addCmdHidden "db-list-units" $ addCmdImpl $ dbListUnitsTask dbPathL
  addCmdHidden "db-graph" $ addCmdImpl $ dbGraphTask dbPathL
  addCmdHidden "plan-info" $ addCmdImpl $ planInfoTask plansFileL
  addCmdHidden "plan-list" $ addCmdImpl $ planListTask plansFileL
  addCmdHidden "plan-roots" $ addCmdImpl $ planRootsTask dbPathL plansFileL
  addCmdHidden "plan-coverage" $ addCmdImpl $ planCoverageTask dbPathL
                                                               plansFileL
  addCmdHidden "roots-coverage" $ addCmdImpl $ rootsCoverageTask dbPathL
                                                                 rootsFileL
  addCmdHidden "registry-coverage" $ addCmdImpl $ registryCoverageTask dbPathL
  addCmdHidden "registry-recheck-plans" $ do
    verbose <- addSimpleBoolFlag "v" ["verbose"] mempty
    addCmdImpl $ registryRecheckPlansTask (verboseGlobal || verbose)
  addCmd "register-dist-newstyle" $ do
    addCmdSynopsis "add dist-newstyle dir to global registry"
    distNewstylePathM <- addParamStringOpt "DISTNEWPATH" paramDirectory
    addCmdImpl $ registerTask distNewstylePathM
  addCmd "store-gc" $ do
    addCmdSynopsis "perform gc on a cabal store"
    addCmdHelp storeGcHelpDoc
    reorderStart
    dry     <- addSimpleBoolFlag [] ["dryrun", "dry-run"] mempty
    verbose <- addSimpleBoolFlag "v" ["verbose"] mempty
    reorderStop
    addCmdImpl $ storeGcTask storepathL
                             ghcpkgL
                             compilerL
                             dbPathL
                             (dryGlobal || dry)
                             (verboseGlobal || verbose)
 where
  versionTask :: IO ()
  versionTask = do
    putStrLn $ "pkgdbgc version " ++ showVersion version
    putStrLn $ "Copyright (C) 2018 Lennart Spitzner"
    putStrLn $ "There is NO WARRANTY, to the extent permitted by law."


helpDoc :: PP.Doc
helpDoc = PP.vcat $ List.intersperse
  (PP.text "")
  [ parDocW
    [ "The intended usage of this program is to garbage-collect (gc) the cabal"
    , "\"store\" package database used with cabal's new-build feature."
    , "The roots for the garbage collection are tracked by looking at the"
    , "`plan.json` file in dist-newstyle directories."
    , "`plan.json` might not be the only sensible source for roots"
    , "(even manual registration might make sense in certain cases) but"
    , "for now there is no explicit support for any other sources."
    ]
  , parDocW
    [ "This program uses a (user-)global registry"
    , "(placed in XDGCONFIGDIR/pkgdbgc/rootregistry.yaml)"
    , "in order to track two things:"
    ]
  , PP.hang (PP.text "") 2 $ PP.vcat
    [ PP.text "1) a set of \"root\" packages"
    , PP.text "2) a set of dist-newstyle build directories"
    ]
  , parDocW
    [ "roots determine the packages to retain during gc"
    , "and the dist-newstyle directories are saved in order to update the"
    , "roots before performing gc."
    ]
  , parDoc "See `help <subcommand>` for the commands listed below."
  , PP.vcat
    [ parDoc "There is NO WARRANTY, to the extent permitted by law."
    , parDoc "This program is free software released under the BSD3 license."
    , parDoc $ "See https://github.com/lspitzner/pkgdbgc"
    , parDoc
    $  "Please report bugs at"
    ++ " https://github.com/lspitzner/pkgdbgc/issues"
    ]
  ]

storeGcHelpDoc :: PP.Doc
storeGcHelpDoc = PP.vcat $ List.intersperse
  (PP.text "")
  [ parDocW
    [ "Traverses known dist-newstyle directories to refresh set of root"
    , "packages and, barring additional flags, performs garbage collection in"
    , "the users .cabal/store directory for the compiler version determined"
    , "from the version of the <ghc-pkg> command."
    ]
  , parDocW
    [ "There is one package-db per compiler version in a store, and the"
    , "internal format of package-dbs varies between versions."
    , "As a consequence, only one package-db can be garbage-collected at a"
    , "time and the user must specify the matching <ghc-pkg> command (for any"
    , "version differing from the <ghc-pkg> version present in PATH)."
    ]
  , parDocW
    [ "Note especially the (top-level) --with-ghc-pkg, --dry-run"
    , "and --verbose flags."
    ]
  ]

parDoc :: String -> PP.Doc
parDoc = PP.fsep . fmap PP.text . List.words

parDocW :: [String] -> PP.Doc
parDocW = PP.fsep . fmap PP.text . List.words . List.unwords

getTermCols :: IO (Maybe Int)
getTermCols =
  (\(_, x, _) -> readMaybe x)
    <$> Process.readCreateProcessWithExitCode (Process.shell "tput cols") ""


addHelpCommand' :: Applicative f => CommandDesc a -> CmdParser f (IO ()) ()
addHelpCommand' = addHelpCommandWith $ \desc -> do
  colsM <- getTermCols
  pure
    $ PP.renderStyle PP.style { PP.ribbonsPerLine = 1.0
                              , PP.lineLength     = fromMaybe 80 colsM
                              }
    $ ppHelpDepthOne desc

