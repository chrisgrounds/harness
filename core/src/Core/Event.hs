{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Core.Event (ServerEvent (..)) where

import Core.Run (Run)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | The server replies to each 'Core.Run.Requirement' with any number of
-- 'RunUpdated' snapshots of that requirement's run, then one 'TurnFinished'.
data ServerEvent
  = RunUpdated Run
  | TurnFinished
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
