{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.Settings (ModelChoice (..), AccountView (..), SettingsSnapshot (..), loadSettings) where

import Control.Exception (IOException, catch, try)
import qualified OpenAI.Http as Http
import Core
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified OpenAI.Auth as Auth
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)

-- Only public metadata is held in the UI. Credentials stay in OpenAI.Auth.
data ModelChoice = ModelChoice
  { choiceModel :: Model, choiceLabel :: Text, choiceAccount :: Maybe Text
  } deriving (Eq, Show, Generic)
instance ToJSON ModelChoice
instance FromJSON ModelChoice

data AccountView = AccountView
  { accountId :: Text, accountLabel :: Text, accountConfigured :: Bool, accountActive :: Bool
  } deriving (Eq, Show)

data SettingsSnapshot = SettingsSnapshot
  { configuredAccounts :: [AccountView], openaiChoices :: [ModelChoice], catalogError :: Maybe Text
  } deriving (Eq, Show)

loadSettings :: IO SettingsSnapshot
loadSettings = do
  accounts <- map (\(key, label, enabled, active) -> AccountView key label enabled active) <$> Auth.accountViews
  dir <- getXdgDirectory XdgConfig "harness/openai"
  let cacheFile = dir </> "catalogs.json"
  exists <- doesFileExist cacheFile
  cached <- if exists then L.readFile cacheFile >>= either (const (fail "Could not read cached model catalogs.")) pure . eitherDecode else pure Map.empty
  let activeAccounts = [a | a <- accounts, accountActive a, accountConfigured a]
  (catalogs, err) <- case activeAccounts of
    [] -> pure (cached, Nothing)
    a : _ -> do
      result <- try (Auth.modelCatalog `catch` oauthError) :: IO (Either IOException [(Text, Text)])
      case result of
        Left err -> pure (cached, Just (T.pack (show err)))
        Right models -> do
          let updated = Map.insert (accountId a) models cached
          writeMetadata cacheFile updated
          pure (updated, Nothing)
  latestAccounts <- map (\(key, label, enabled, active) -> AccountView key label enabled active) <$> Auth.accountViews
  pure (SettingsSnapshot latestAccounts
    [ModelChoice (OpenAIModel slug) (label <> " · " <> accountLabel a) (Just (accountId a))
     | a <- latestAccounts, accountConfigured a,
       (slug,label) <- Map.findWithDefault [] (accountId a) catalogs] err)

writeMetadata :: ToJSON a => FilePath -> a -> IO ()
writeMetadata file value = do
  dir <- getXdgDirectory XdgConfig "harness/openai"
  (temp, handle) <- openBinaryTempFile dir "models.tmp"
  L.hPut handle (encode value)
  hClose handle
  renameFile temp file

oauthError :: Http.OAuthFailure -> IO a
oauthError = fail . show
