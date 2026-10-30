{-# LANGUAGE OverloadedStrings #-}

module Agent
  ( Llm,
    loadLlm,
    runAgent,
    toolMessage,
  )
where

import Anthropic (anthropicLlm)
import Control.Concurrent (newQSem, signalQSem, waitQSem)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (bracket_)
import Core
import Data.Text (Text)
import qualified Data.Text as T
import LoadEnv
import Inception (inceptionLlm)
import OpenAI (openaiLlm)
import System.Environment (lookupEnv)
import System.Exit (die)

type Llm = Chat -> IO Chat

loadLlm :: Model -> IO Llm
loadLlm m = do
  loadEnv
  case m of
    OpenAIModel _ -> openaiLlm Nothing
    Mercury3 -> inceptionLlm . T.pack <$> key "INCEPTION_API_KEY"
    _ -> anthropicLlm . T.pack <$> key "ANTHROPIC_API_KEY"
  where
    key name = lookupEnv name >>= maybe (die ("Set " <> name <> " to use " <> T.unpack (modelId m) <> ".")) pure

-- Stops without another model call once finished reports True after a round of tools.
runAgent :: Llm -> Int -> IO Bool -> (ToolRequest -> IO Text) -> Chat -> IO Chat
runAgent llm budget finished handle chat
  | budget <= 0 = pure chat
  | otherwise = do
      replied <- llm chat
      case getLatestToolRequests replied of
        [] -> pure replied
        requests -> do
          results <- runTools (\r -> toolMessage (toolRequestId r) <$> handle r) requests
          let next = addMessages (reverse results) replied
          done <- finished
          if done then pure next else runAgent llm (budget - 1) finished handle next

-- Reads in the same reply may overlap. State-changing calls form barriers,
-- so later reads observe earlier changes and multiple updates stay ordered.
runTools :: (ToolRequest -> IO a) -> [ToolRequest] -> IO [a]
runTools _ [] = pure []
runTools handle requests = case span readOnly requests of
  ([], request : rest) -> do
    result <- handle request
    (result :) <$> runTools handle rest
  (readRequests, rest) -> do
    slots <- newQSem 4
    results <- mapConcurrently (\request -> bracket_ (waitQSem slots) (signalQSem slots) (handle request)) readRequests
    (results ++) <$> runTools handle rest
  where
    readOnly request = toolRequestFunctionParamsName (toolRequestFunction request) `elem` [ReadFile, ListFiles, RipGrep]

toolMessage :: ToolCallId -> Text -> Message
toolMessage callId result = (textMessage Tool result) {toolCallId = Just callId}
