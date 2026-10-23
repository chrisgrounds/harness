{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Chat
import Control.Exception (IOException, bracket, try)
import Control.Monad (forever, unless)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import LoadEnv
import Network.HTTP.Req
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import OpenAI
import System.Directory
import System.Environment
import System.Process (system)

newtype BaseUrl = BaseUrl Text
  deriving (Show)

newtype Uri = Uri Text
  deriving (Show)

newtype ApiKey = ApiKey String
  deriving (Show)

newtype MaxTurns = MaxTurns Int
  deriving stock (Show, Eq)
  deriving newtype (Num)

-- @spec can we make this a FilePath instead of Text?
data ReadFilePath = ReadFilePath {readPath :: Text}
  deriving (Show)

instance FromJSON ReadFilePath where
  parseJSON = withObject "ReadFilePath" $ \value ->
    ReadFilePath <$> value .: "path"

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
  print "opening server, listening..."

  let model = Mercury3
  let chat =
        ChatPayload
          []
          model
          [ ToolDefinition
              { toolType = FunctionTool,
                toolFunction =
                  ToolFunction
                    { toolFunctionName = ReadFile,
                      toolFunctionDescription = "Reads a file",
                      toolFunctionParameters =
                        object
                          [ "type" .= ("string" :: Text),
                            "properties"
                              .= object
                                [ "path"
                                    .= object
                                      [ "type" .= ("string" :: Text),
                                        "description" .= ("The file path to read" :: Text)
                                      ]
                                ],
                            "required" .= (["path"] :: [Text]),
                            "additionalProperties" .= False
                          ]
                    }
              }
          ]

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

talk :: Socket -> ChatPayload -> ApiKey -> IO ()
talk s chat apiKey = do
  bytes <- recv s 1024
  unless (S.null bytes) $ do
    message <- either fail pure (eitherDecodeStrict bytes :: Either String Message)

    let newChat = addMessage message chat

    newerChat <- callLLM apiKey newChat

    chatWithToolCalls <- talkWithToolRequests apiKey (getLatestToolRequests newerChat) newerChat

    sendAll s $ L.toStrict (encode chatWithToolCalls)
    talk s chatWithToolCalls apiKey

talkWithToolRequests :: ApiKey -> [ToolRequest] -> ChatPayload -> IO ChatPayload
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
runToolRequest tool = case (toolRequestFunctionParamsName toolDetails) of
  ReadFile -> do
    res <- runReadFile toolDetails

    case res of
      Left err -> pure (toolRequestId tool, "Could not read file: " <> T.pack (show err))
      Right contents -> pure (toolRequestId tool, contents)
  where
    toolDetails = toolRequestFunction tool

runReadFile :: ToolRequestFunctionParams -> IO (Either IOException Text)
runReadFile toolDetails = do
  let args = toolRequestFunctionParamsArguments toolDetails
  ReadFilePath path <-
    either fail pure (eitherDecodeStrict (TE.encodeUtf8 args))
  -- For now we make this a relative path
  -- we should really handle this much better
  try $ TIO.readFile ("../" <> T.unpack path)

callLLM :: ApiKey -> ChatPayload -> IO ChatPayload
callLLM key@(ApiKey apiKey) chat = do
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
