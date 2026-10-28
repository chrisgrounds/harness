{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Core.Event (StartRun (..), ServerEvent (..)) where

import Core.Run (Requirement, Run)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Core.Model (Model)

-- | What the client sends: a requirement to run against a cabal project.
data StartRun = StartRun
  { -- | Absolute, since the server's working directory is unrelated.
    runProject :: FilePath,
    runRequirement :: Requirement
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | On each new connection the server first sends one 'Connected' naming the
-- model it runs with. It then replies to each 'StartRun' with any number of
-- 'RunUpdated' snapshots of that requirement's run, then one 'TurnFinished'.
data ServerEvent
  = Connected Model
  | RunUpdated Run
  | TurnFinished
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
