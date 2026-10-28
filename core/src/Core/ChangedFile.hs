{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.ChangedFile
  ( ChangeKind (..),
    ChangedFile (..),
    changeLabel,
    changeTag,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Whether a file was created by the run or already existed.
data ChangeKind
  = -- | The file did not exist before the run.
    NewFile
  | -- | The file existed before the run; the run edited it.
    ExistingFile
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | A file the run touched, with whether it was new.
data ChangedFile = ChangedFile
  { -- | Relative to the project root.
    changedPath :: FilePath,
    changedKind :: ChangeKind
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | The bracket label shown before a changed file's name.
changeLabel :: ChangeKind -> Text
changeLabel NewFile = "write"
changeLabel ExistingFile = "edit"

-- | A changed file as shown in transcripts and panels, as @[write] path@ or
-- @[edit] path@.
changeTag :: ChangedFile -> Text
changeTag (ChangedFile path kind) = "[" <> changeLabel kind <> "] " <> T.pack path
