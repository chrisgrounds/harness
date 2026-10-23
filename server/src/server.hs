{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Chat
import Control.Exception
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
          [ Tool
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

    agentMessage <- callLLM message apiKey chat

    let chatPayloadWithAgentResponse = addMessage agentMessage . addMessage message $ chat

    sendAll s $ L.toStrict (encode chatPayloadWithAgentResponse)

    (_, ToolCallResult toolCallResults) <- handleAssistantMessage agentMessage

    print $ toolCallResults

    let newChatPayload = chatPayloadWithAgentResponse

    -- sendAll s $ L.toStrict (encode newChatPayload)
    talk s newChatPayload apiKey

data ToolCallResult = ToolCallResult [(Text, Text)]

handleAssistantMessage :: Message -> IO (Message, ToolCallResult)
handleAssistantMessage msg = case role msg of
  Assistant -> do
    let tools = toolCalls msg

    -- we can run them in parallel later on, but for now one at a time
    res <- mapM runToolRequest tools

    pure (msg, ToolCallResult res)
  _ -> pure (msg, ToolCallResult [])

runToolRequest :: ToolRequest -> IO (Text, Text)
runToolRequest tool = case (toolRequestFunctionParamsName toolDetails) of
  ReadFile -> do
    res <- runReadFile toolDetails
    pure (toolRequestId tool, res)
  where
    toolDetails = toolRequestFunction tool

runReadFile :: ToolRequestFunctionParams -> IO Text
runReadFile toolDetails = do
  let args = toolRequestFunctionParamsArguments toolDetails
  ReadFilePath path <-
    either fail pure (eitherDecodeStrict (TE.encodeUtf8 args))
  -- For now we make this a relative path
  TIO.readFile ("./" <> T.unpack path)

callLLM :: Message -> ApiKey -> ChatPayload -> IO Message
callLLM message key@(ApiKey apiKey) chat = do
  let ApiDefinition url = toApiDefinition (model chat)

  runReq defaultHttpConfig $ do
    let userPayload =
          ChatPayload
            { messages = [Message User (content message) []],
              model = (model chat),
              tools = (tools chat)
            }

    res <- req POST url (ReqBodyJson userPayload) jsonResponse (header "Authorization" ("Bearer " <> B.pack apiKey))

    let parsedRes = responseBody res :: OpenAIChatCompletionResponse

    case listToMaybe (responseChoices parsedRes) of
      Nothing -> pure $ Message Assistant (Just "No content") []
      Just choice -> do
        let responseMessage = choiceMessage choice
        pure $
          Message
            { role = messageRole responseMessage,
              content = messageContent responseMessage,
              toolCalls = fromMaybe [] (messageToolCalls responseMessage)
            }
