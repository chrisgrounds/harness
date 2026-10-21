{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

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
  content :: String
} deriving (Show, Generic, ToJSON, FromJSON)

addMessage :: Message -> ChatPayload -> ChatPayload
addMessage m c = ChatPayload (m : messages c) (model c)

data Role = User | Agent | System
  deriving (Show, Generic, ToJSON, FromJSON)

data Model = Mercury3
  deriving (Show, Generic)

instance ToJSON Model where
  toJSON Mercury3 = object ["model" .= ("mercury-3" :: Text)]

instance FromJSON Model where
  parseJSON = withObject "model" $ \v -> do
    model <- v .: "model"
    case (model :: Text) of
      "mercury-3" -> pure Mercury3
      _           -> fail "unknown model"

data ApiDefinition = ApiDefinition BaseUrl Uri
 deriving (Show)

toApiDefinition :: Model -> ApiDefinition
toApiDefinition Mercury3 = ApiDefinition (BaseUrl "api.inceptionlabs.ai") (Uri "/v1/chat/completions")

-- TODO:
-- handle tool calls
-- send response to client
-- build a test client

main :: IO ()
main = do
  loadEnv
  apiKey <- ApiKey <$> getEnv "INCEPTION_API_KEY"
  let socketPath = "/tmp/harness.sock"

  let model = Mercury3
  let chat = ChatPayload [] model

  bracket
    (socket AF_UNIX Stream defaultProtocol)
    (\s -> do
      close s
      removeFile socketPath)
    (\s -> do
      bind s (SockAddrUnix socketPath)

      listen s 1

      (conn, _) <- accept s
      talk conn chat apiKey
      close conn
    )
  -- run_agent (MaxTurns 1) apiKey model

talk :: Socket -> ChatPayload -> ApiKey -> IO ()
talk s chat apiKey = do
  bytes <- recv s 1024
  unless (S.null bytes) $ do
    message <- either fail pure (eitherDecodeStrict bytes :: Either String Message)

    agentMessage <- callLLM message apiKey (model chat)

    sendAll s $ "you said: " <> bytes
    talk s (addMessage agentMessage . addMessage message $ chat) apiKey

callLLM :: Message -> ApiKey -> Model -> IO Message
callLLM message key@(ApiKey apiKey) model = do
  let ApiDefinition (BaseUrl baseUrl) (Uri uri) = toApiDefinition model

  runReq defaultHttpConfig $ do
    let payload = ChatPayload { messages = [Message User (content message)], model = model}

    res <- req POST (https baseUrl /: uri) (ReqBodyJson payload) jsonResponse (header "Authorization" ("Bearer " <> B.pack apiKey))

    pure $ Message Agent (L.unpack (encode (responseBody res :: Value)))
