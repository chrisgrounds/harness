{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Network.Socket
import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import System.Environment
import LoadEnv
import System.Directory
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as L
import GHC.Generics (Generic)
import Data.Text
import Control.Monad (unless, forever, void)
import Network.Socket.ByteString (recv, sendAll)
import Data.Aeson.Key (fromString)
import Control.Exception
import Data.Maybe (listToMaybe)
import Control.Monad (join)

newtype BaseUrl = BaseUrl Text
  deriving (Show)

newtype Uri = Uri Text
  deriving (Show)

newtype ApiKey = ApiKey String
  deriving (Show)

newtype MaxTurns = MaxTurns Int
  deriving stock (Show, Eq)
  deriving newtype (Num)

data ChatPayload = ChatPayload
  { messages :: [Message]
  , model    :: Model
  } deriving (Show, Generic, ToJSON, FromJSON)

data Message = Message {
  role :: Role,
  content :: Maybe Text
} deriving (Show, Generic, ToJSON, FromJSON)

addMessage :: Message -> ChatPayload -> ChatPayload
addMessage m c = ChatPayload (m : messages c) (model c)

data Role = User | Agent | System
  deriving (Show, Generic)

instance ToJSON Role where
  toJSON User   = String "user"
  toJSON Agent  = String "assistant"
  toJSON System = String "system"

instance FromJSON Role where
  parseJSON = withText "Role" $ \value ->
    case value of
      "user"      -> pure User
      "assistant" -> pure Agent
      "system"    -> pure System
      _           -> fail "unknown role"

data Model = Mercury3
  deriving (Show, Generic)

data OpenAIChatCompletionResponse = OpenAIChatCompletionResponse
  { responseChoices          :: [OpenAIChatResponseChoice]
  , responseCreated          :: Integer
  , responseId               :: Text
  , responseModel            :: Text
  , responseObject           :: Text
  , responseReasoningSummary :: Maybe Value
  , responseUsage            :: OpenAIChatResponseUsage
  , responseWarning          :: Maybe Value
  } deriving (Show, Generic)

data OpenAIChatResponseChoice = OpenAIChatResponseChoice
  { choiceFinishReason :: Maybe Text
  , choiceIndex        :: Int
  , choiceMessage      :: OpenAIChatResponseMessage
  } deriving (Show, Generic)

data OpenAIChatResponseMessage = OpenAIChatResponseMessage
  { messageContent    :: Maybe Text
  , messageRole       :: Role
  , messageToolCallId :: Maybe Text
  , messageToolCalls  :: Maybe [Value]
  } deriving (Show, Generic)

data OpenAIChatResponseUsage = OpenAIChatResponseUsage
  { usageCompletionTokens        :: Int
  , usageCompletionTokensDetails :: OpenAIChatCompletionTokensDetails
  , usagePromptTokens            :: Int
  , usagePromptTokensDetails     :: OpenAIChatPromptTokensDetails
  , usageTotalTokens             :: Int
  } deriving (Show, Generic)

data OpenAIChatCompletionTokensDetails = OpenAIChatCompletionTokensDetails
  { completionReasoningTokens :: Int
  } deriving (Show, Generic)

data OpenAIChatPromptTokensDetails = OpenAIChatPromptTokensDetails
  { promptCachedTokens :: Int
  } deriving (Show, Generic)


jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions
  { fieldLabelModifier =
      camelTo2 '_' . Prelude.drop (Prelude.length prefix)
  }

instance FromJSON OpenAIChatCompletionResponse where
  parseJSON = genericParseJSON (jsonOptions "response")

instance FromJSON OpenAIChatResponseChoice where
  parseJSON = genericParseJSON (jsonOptions "choice")

instance FromJSON OpenAIChatResponseMessage where
  parseJSON = genericParseJSON (jsonOptions "message")

instance FromJSON OpenAIChatResponseUsage where
  parseJSON = genericParseJSON (jsonOptions "usage")

instance FromJSON OpenAIChatCompletionTokensDetails where
  parseJSON = genericParseJSON (jsonOptions "completion")

instance FromJSON OpenAIChatPromptTokensDetails where
  parseJSON = genericParseJSON (jsonOptions "prompt")


instance ToJSON Model where
  toJSON Mercury3 = String "mercury-3"

instance FromJSON Model where
  parseJSON = withObject "model" $ \v -> do
    model <- v .: "Model"
    case (model :: Text) of
      "mercury-3" -> pure Mercury3
      _           -> fail "unknown model"

data ApiDefinition = ApiDefinition (Url 'Https)
 deriving (Show)

toApiDefinition :: Model -> ApiDefinition
toApiDefinition Mercury3 = ApiDefinition (https "api.inceptionlabs.ai" /: "v1" /: "chat" /: "completions")

-- TODO:
-- extract from the OpenAIChatCompletionResponse type relevant data and store in ChatPayload.Message
-- send chatpayload to client
-- only send data we want to the llm
-- handle tool calls

withSocket :: FilePath ->  (Socket -> IO b) -> IO b
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
  print "listening..."

  let model = Mercury3
  let chat = ChatPayload [] model

  withSocket socketPath
    (\s -> do
      bind s (SockAddrUnix socketPath)

      listen s 1

      (conn, _) <- accept s
      talk conn chat apiKey
      close conn
    )

getContent :: OpenAIChatCompletionResponse -> Maybe Text
getContent res =
  join $ (messageContent . choiceMessage) <$> (listToMaybe $ responseChoices res)

talk :: Socket -> ChatPayload -> ApiKey -> IO ()
talk s chat apiKey = do
  bytes <- recv s 1024
  unless (S.null bytes) $ do
    message <- either fail pure (eitherDecodeStrict bytes :: Either String Message)

    agentMessage <- callLLM message apiKey (model chat)

    let newChatPayload = addMessage agentMessage . addMessage message $ chat
    sendAll s $ L.toStrict (encode newChatPayload)
    talk s newChatPayload apiKey

callLLM :: Message -> ApiKey -> Model -> IO Message
callLLM message key@(ApiKey apiKey) model = do
  let ApiDefinition url = toApiDefinition model

  runReq defaultHttpConfig $ do
    let payload = ChatPayload { messages = [Message User (content message)], model = model }

    res <- req POST url (ReqBodyJson payload) jsonResponse (header "Authorization" ("Bearer " <> B.pack apiKey))

    let parsedRes = responseBody res :: OpenAIChatCompletionResponse

    let content = getContent parsedRes

    pure $ Message Agent content
