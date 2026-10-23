{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Chat
import Control.Exception (bracket)
import Control.Monad (forever, unless)
import Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import LoadEnv
import Network.HTTP.Req
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import OpenAI
import System.Directory
import System.Environment
import Tools

newtype BaseUrl = BaseUrl Text
  deriving (Show)

newtype Uri = Uri Text
  deriving (Show)

newtype ApiKey = ApiKey String
  deriving (Show)

data ApiDefinition = ApiDefinition (Url 'Https)
  deriving (Show)

toApiDefinition :: Model -> ApiDefinition
toApiDefinition Mercury3 = ApiDefinition (https "api.inceptionlabs.ai" /: "v1" /: "chat" /: "completions")

withSocket :: FilePath -> (Socket -> IO b) -> IO b
withSocket path io =
  bracket
    (socket AF_UNIX Stream defaultProtocol)
    (\s -> close s >> removeFile path)
    io

main :: IO ()
main = do
  loadEnv
  apiKey <- ApiKey <$> getEnv "INCEPTION_API_KEY"
  let socketPath = "/tmp/harness.sock"
  putStrLn "opening server, listening..."

  -- Don't hardcode this
  let llmModel = Mercury3
  let chat =
        Chat
          []
          llmModel
          (map toolToApiDefinition [ReadFile, Discover])

  withSocket
    socketPath
    ( \s -> do
        bind s (SockAddrUnix socketPath)

        listen s 1

        forever $ do
          (conn, addr) <- accept s
          print $ "accepting connection from: " <> show addr
          talk conn chat apiKey
          close conn
    )

talk :: Socket -> Chat -> ApiKey -> IO ()
talk s chat apiKey = do
  bytes <- recv s 1024
  unless (S.null bytes) $ do
    message <- either fail pure (eitherDecodeStrict bytes :: Either String Message)

    -- wtf are these names, Chris
    let newChat = addMessage message chat

    newerChat <- callLLM apiKey newChat

    chatWithToolCalls <- talkWithToolRequests apiKey (getLatestToolRequests newerChat) newerChat

    sendAll s $ L.toStrict (encode chatWithToolCalls)
    talk s chatWithToolCalls apiKey

talkWithToolRequests :: ApiKey -> [ToolRequest] -> Chat -> IO Chat
talkWithToolRequests _ [] chat = pure chat
talkWithToolRequests apiKey toolRequests chat = do
  toolCallResults <- mapM runToolRequest toolRequests

  if length toolCallResults == 0
    then pure chat
    else do
      let toolMessages =
            [ Message
                { role = Tool,
                  content = Just toolResult,
                  toolCalls = [],
                  toolCallId = Just tCallId
                }
            | (tCallId, toolResult) <- toolCallResults
            ]

      let newChat = addMessages toolMessages chat

      chatWithAgentResponse <- callLLM apiKey newChat

      talkWithToolRequests apiKey (maybe [] toolCalls (listToMaybe $ messages chatWithAgentResponse)) chatWithAgentResponse

runToolRequest :: ToolRequest -> IO (ToolCallId, Text)
runToolRequest tool = do
  let toolDetails = toolRequestFunction tool
  res <- runTool (toolRequestFunctionParamsName toolDetails) toolDetails

  case res of
    -- this error message is specific for ReadFile and does not apply to Discover, for example
    Left err -> pure (toolRequestId tool, "Could not read file: " <> T.pack (show err))
    Right contents -> pure (toolRequestId tool, contents)

callLLM :: ApiKey -> Chat -> IO Chat
callLLM (ApiKey apiKey) chat = do
  let ApiDefinition url = toApiDefinition (model chat)

  -- the `Chat` type here stores most recent messages at the beginning of [Message]
  -- whereas the LLM expects the reverse
  let llmChat = chat {messages = reverse $ messages chat}

  runReq defaultHttpConfig $ do
    res <- req POST url (ReqBodyJson llmChat) jsonResponse (header "Authorization" ("Bearer " <> B.pack apiKey))
    let parsedRes = responseBody res :: OpenAIChatCompletionResponse

    case listToMaybe (responseChoices parsedRes) of
      Nothing -> pure chat
      Just choice -> do
        let responseMessage = choiceMessage choice
        let msg =
              Message
                { role = messageRole responseMessage,
                  content = messageContent responseMessage,
                  toolCalls = fromMaybe [] (messageToolCalls responseMessage),
                  toolCallId = Nothing
                }

        pure $ addMessage msg chat
