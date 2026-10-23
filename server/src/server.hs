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
import Control.Monad (unless)
import Network.Socket.ByteString (recv, sendAll)
import Control.Exception
import Data.Maybe (listToMaybe)
import Control.Monad (join)
import OpenAI

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

data Model = Mercury3
  deriving (Show, Generic)

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
