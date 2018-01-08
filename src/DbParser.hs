{-# LANGUAGE CPP, ScopedTypeVariables #-}
module DbParser (getPkgInfos, (.>)) where



import           Prelude
import qualified System.Info
import qualified Config

import           Data.List
import           Data.Maybe
import           Control.Monad
import           Distribution.InstalledPackageInfo
import           Distribution.ModuleName

import           System.Directory
import           System.Environment             ( getEnv )
import           System.FilePath
import           System.IO
import qualified Control.Exception             as Exc

import           GHC.Paths

import qualified Control.Exception             as Exception
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class




(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)
infixl 9 .>

getPkgInfos :: FilePath -> IO (Either String [InstalledPackageInfo])
getPkgInfos pkgDir = runExceptT $ do
  do -- check if input dir exists
    pkgDirExists <- liftIO $ doesDirectoryExist pkgDir
    unless pkgDirExists $ throwE "input path does not exist"
  confFiles <- do
    files <- liftIO $ getDirectoryContents pkgDir
    pure $ [ pkgDir </> file | file <- files, ".conf" `isSuffixOf` file ]
  confContents <- liftIO $ confFiles `forM` readUTF8File
  infos        <- forM confContents $ parseInstalledPackageInfo .> \case
    ParseFailed err  -> throwE $ "conf parser error: " ++ show err
    ParseOk _warns r -> pure r
  pure infos

readUTF8File :: FilePath -> IO String
readUTF8File file = do
  h <- openFile file ReadMode
  -- fix the encoding to UTF-8
  hSetEncoding h utf8
  Exc.catch
    (hGetContents h)
    ( \(err :: Exc.IOException) -> do
      print err
      hClose h
      h' <- openFile file ReadMode
      hSetEncoding h' localeEncoding
      hGetContents h'
    )
