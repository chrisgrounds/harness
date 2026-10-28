{-# LANGUAGE OverloadedStrings #-}

-- | A stand-in for the model that replies from a script, so runs can be
-- tested without network access.
module ScriptedModel (Reply (..), scriptedLlm, latestText, availableTools) where

import Agent (Llm)
import Core
import Data.Aeson (Value, encode)
import Data.IORef (IORef, atomicModifyIORef')
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as TE

data Reply = CallTool ToolName Value | Say Text

-- | The counter numbers tool calls; it is shared because agents run concurrently.
scriptedLlm :: IORef Int -> (Chat -> IO Reply) -> Llm
scriptedLlm counter decide chat = do
  n <- atomicModifyIORef' counter (\x -> (x + 1, x))
  reply <- decide chat
  let message = case reply of
        Say text -> textMessage Assistant text
        CallTool name args ->
          Message
            Assistant
            Nothing
            [ToolRequest ("call-" <> T.pack (show n)) FunctionTool (ToolRequestFunctionParams name (TE.decodeUtf8 (L.toStrict (encode args))))]
            Nothing
            Nothing
            Nothing
  -- Every reply carries a token rate, as the real models' do.
  pure (addMessage message {tokensPerSecond = Just 10} chat)

-- | The content of the most recent message, with its role.
latestText :: Chat -> Maybe (Role, Text)
latestText chat = listToMaybe (messages chat) >>= \m -> (,) (role m) <$> content m

availableTools :: Chat -> [ToolName]
availableTools = map (toolFunctionName . toolFunction) . tools
