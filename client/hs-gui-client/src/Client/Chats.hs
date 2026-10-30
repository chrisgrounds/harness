{-# LANGUAGE OverloadedStrings #-}

-- | Chats kept across sessions: one versioned JSON file of every project's
-- chats, replaced atomically on each save.
module Client.Chats
  ( SavedChat (..),
    chatsFile,
    loadChats,
    saveChats,
    restamp,
    urgentChange,
  )
where

import Client.Settings (ModelChoice)
import Control.Exception (IOException, try)
import Control.Monad (when)
import Core (ReasoningEffort, Run (..))
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import System.Directory
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openBinaryTempFile)

data SavedChat = SavedChat
  { chatId :: Int, projectPath :: FilePath, composerDraft :: Text,
    modelChoice :: Maybe ModelChoice, reasoningEffort :: Maybe ReasoningEffort,
    runs :: [Run], inputHistory :: [Text], lastUpdated :: UTCTime,
    customTitle :: Maybe Text
  } deriving (Eq, Show)

instance ToJSON SavedChat where
  toJSON c = object ["id" .= chatId c, "project" .= projectPath c, "draft" .= composerDraft c, "modelChoice" .= modelChoice c, "reasoningEffort" .= reasoningEffort c, "runs" .= runs c, "inputHistory" .= inputHistory c, "lastUpdated" .= lastUpdated c, "customTitle" .= customTitle c]

instance FromJSON SavedChat where
  parseJSON = withObject "SavedChat" $ \o -> SavedChat <$> o .: "id" <*> o .: "project" <*> o .:? "draft" .!= "" <*> o .:? "modelChoice" <*> o .:? "reasoningEffort" <*> o .: "runs" <*> o .:? "inputHistory" .!= [] <*> o .: "lastUpdated" <*> o .:? "customTitle"

-- | The file's top level, so later formats can read and migrate this one.
newtype ChatsFile = ChatsFile [SavedChat]

currentVersion :: Int
currentVersion = 1

instance ToJSON ChatsFile where
  toJSON (ChatsFile chats) = object ["version" .= currentVersion, "chats" .= chats]

instance FromJSON ChatsFile where
  parseJSON = withObject "ChatsFile" $ \o -> do
    version <- o .: "version"
    when (version /= currentVersion) (fail ("unsupported chats file version " <> show (version :: Int)))
    ChatsFile <$> o .: "chats"

chatsFile :: IO FilePath
chatsFile = getXdgDirectory XdgData ("harness" </> "chats.json")

-- | Every saved chat, of every project. A missing file has none; one that
-- cannot be read is an error, so that saving does not replace it.
loadChats :: FilePath -> IO (Either String [SavedChat])
loadChats file = do
  exists <- doesFileExist file
  if not exists
    then pure (Right [])
    else do
      contents <- try (L.readFile file) :: IO (Either IOException L.ByteString)
      pure $ case contents of
        Left err -> Left (show err)
        Right bytes -> (\(ChatsFile chats) -> chats) <$> eitherDecode bytes

-- | Replaces the project's chats with these, keeping other projects'.
saveChats :: FilePath -> FilePath -> [SavedChat] -> IO ()
saveChats file project chats = do
  existing <- either (const []) id <$> loadChats file
  let others = filter ((/= project) . projectPath) existing
      dir = takeDirectory file
  createDirectoryIfMissing True dir
  (temp, handle) <- openBinaryTempFile dir "chats.tmp"
  L.hPut handle (encode (ChatsFile (others ++ chats)))
  hClose handle
  renameFile temp file

-- | Keeps each chat's earlier time unless something else about it changed.
restamp :: [SavedChat] -> [SavedChat] -> [SavedChat]
restamp before = map keep
  where
    earlier = Map.fromList [(chatId c, c) | c <- before]
    keep c = case Map.lookup (chatId c) earlier of
      Just old | old {lastUpdated = lastUpdated c} == c -> old
      _ -> c

-- | Whether the change is worth saving at once: anything but a draft edit
-- or progress within a step, which can wait to be saved with later ones.
urgentChange :: [SavedChat] -> [SavedChat] -> Bool
urgentChange before after = map outline before /= map outline after
  where
    outline c = (chatId c, modelChoice c, reasoningEffort c, inputHistory c, map (\r -> (iteration r, steps r)) (runs c), customTitle c)
