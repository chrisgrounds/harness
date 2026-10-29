{-# LANGUAGE OverloadedStrings #-}

module Client.Commands (Command (..), parseCommand) where

import Core (Requirement (..))
import Data.Text (Text, strip)

-- | A composed message, once its leading '/' commands are interpreted.
data Command
  = -- | Exit the client.
    Quit
  | -- | Show every tool the harness can call.
    ListTools
  | -- | Send the text, a fresh requirement, to the server.
    RunRequirement Requirement
  deriving (Eq, Show)

-- | Interpret a composed message: '/quit' quits, '/tools' lists the
-- available tools, and anything else starts a run with the whole message as
-- its requirement. The two commands match exactly, case-sensitively, after
-- trimming surrounding whitespace.
parseCommand :: Text -> Command
parseCommand t = case strip t of
  "/quit" -> Quit
  "/tools" -> ListTools
  _ -> RunRequirement (Requirement t)
