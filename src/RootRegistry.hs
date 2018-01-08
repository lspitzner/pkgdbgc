{-# LANGUAGE DeriveGeneric #-}
module RootRegistry
  ( getRegistryRoots
  , addRegistryRoot
  , RootKey(..)
  , UnitId(..)
  , Reason(..)
  )
where



import           Prelude

import           Data.Map                      as Map
import           Data.Set                      as Set
import qualified Data.Text                     as Text
import qualified System.Directory              as Directory
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           System.FilePath
import           Control.Monad

import qualified Data.Aeson.Types              as Aeson
import           Data.Yaml
import           Text.Read                      ( readMaybe )

import qualified Distribution.Types.UnitId     as Cabal
import           Distribution.Text              ( disp )

import           GHC.Generics

import qualified Cabal.Plan



data RootRegistry = RootRegistry (Map RootKey [UnitId])
  deriving Generic

instance FromJSON RootRegistry where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptions
instance ToJSON RootRegistry where
  toJSON = Aeson.genericToJSON aesonDecodeOptions
  toEncoding = Aeson.genericToEncoding aesonDecodeOptions

data RootKey = RootKey
  { _rk_location :: FilePath
  , _rk_compiler :: Cabal.Plan.PkgId
  , _rk_reason :: Reason
  }
  deriving (Generic, Eq, Ord)

instance FromJSON RootKey where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptions
instance ToJSON RootKey where
  toJSON = Aeson.genericToJSON aesonDecodeOptions
  toEncoding = Aeson.genericToEncoding aesonDecodeOptions

instance Aeson.FromJSONKey RootKey
instance Aeson.ToJSONKey RootKey

newtype UnitId = UnitId Cabal.UnitId

instance FromJSON UnitId where
  parseJSON = withText
    "UnitId"
    ( \txt -> case readMaybe ("UnitId \"" ++ Text.unpack txt ++ "\"") of
      Nothing -> fail "could not parse UnitId"
      Just r  -> pure $ UnitId r
    )
instance ToJSON UnitId where
  toJSON (UnitId uid) = String $ Text.pack $ show $ disp uid

data Reason
  = Unspecified
  | Build -- unitId is references from some new-style build directory
  | Install -- unitId is references from new-installed library or dynamically
            -- linked executable
  deriving (Eq, Ord, Generic)

instance FromJSON Reason where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptions
instance ToJSON Reason where
  toJSON = Aeson.genericToJSON aesonDecodeOptions
  toEncoding = Aeson.genericToEncoding aesonDecodeOptions


aesonDecodeOptions :: Aeson.Options
aesonDecodeOptions = Aeson.defaultOptions
  { Aeson.omitNothingFields  = True
  , Aeson.fieldLabelModifier = dropWhile (== '_')
  }


getRegistryRoots :: IO (Either String [Cabal.UnitId])
getRegistryRoots = runExceptT $ do
  userPathXdg <- liftIO
    $ Directory.getXdgDirectory Directory.XdgConfig "pkgdbgc"
  let filePath = userPathXdg </> "rootregister.yaml"
  exists <- liftIO $ Directory.doesFileExist filePath
  unless exists $ throwE $ "registry file not found in " ++ filePath
  loaded <- liftIO $ decodeFile filePath
  case loaded of
    Nothing -> throwE $ "could not decode registry file " ++ filePath
    Just (RootRegistry registry) ->
      pure
        $ Set.toList
        $ Set.fromList
        $ [ unitId | descs <- Map.elems registry, UnitId unitId <- descs ]

addRegistryRoot :: RootKey -> [UnitId] -> IO (Either String ())
addRegistryRoot rootKey descrs = runExceptT $ do
  userPathXdg <- liftIO
    $ Directory.getXdgDirectory Directory.XdgConfig "pkgdbgc"
  let filePath = userPathXdg </> "rootregister.yaml"
  exists   <- liftIO $ Directory.doesFileExist filePath
  registry <- if exists
    then do
      registryM <- liftIO $ decodeFile filePath
      case registryM of
        Nothing -> throwE $ "could not decode registry file " ++ filePath
        Just (RootRegistry registry) -> pure registry
    else do
      liftIO $ Directory.createDirectoryIfMissing True userPathXdg
      pure Map.empty
  let registry' = Map.insert rootKey descrs registry
  liftIO $ encodeFile filePath registry'
  pure ()

