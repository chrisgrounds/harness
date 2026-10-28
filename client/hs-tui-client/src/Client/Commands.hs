{-# LANGUAGE OverloadedStrings #-}

module Client.Commands (Command (..), parseCommand) where

import Core (Requirement (..))
import Data.Text (Text, strip)

-- | A composed input line, once its leading '/' commands are interpreted.
data Command
  = -- | Exit the client.
    Quit
  | -- | Show every tool the harness can call, using the accent colour.
    ListTools
  | -- | Send the text, a fresh requirement, to the server.
    RunRequirement Requirement
  deriving (Eq, Show)

-- | Interpret a composed input line: '/quit' quits, '/tools' lists the available tools, and any other non-empty line starts a run with the whole line as its requirement. The two slash commands match exactly, case-sensitively, after trimming surrounding whitespace.
parseCommand :: Text -> Command
parseCommand t =
  let t' = strip t
  in if t' == "/quit" then Quit
     else if t' == "/tools" then ListTools
     else RunRequirement (Requirement t)

