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
-- save history
-- handle tool calls
-- send response to client
-- build a test client

main :: IO ()
main = do
  loadEnv
  apiKey <- ApiKey <$> getEnv "INCEPTION_API_KEY"

  let model = Mercury3
  let socketPath = "/tmp/harness.sock"


  bracket
    (socket AF_UNIX Stream defaultProtocol)
    (\s -> do
      close s
      removeFile socketPath)
    (\s -> do
      bind s (SockAddrUnix socketPath)

      listen s 1

      -- forever $ do
      (conn, _) <- accept s
      talk conn model
      close conn
    )
  -- run_agent (MaxTurns 1) apiKey model


talk :: Socket -> Model -> IO ()
talk s model = do
  bytes <- recv s 1024
  unless (S.null bytes) $ do
    message <- either fail pure (eitherDecodeStrict bytes :: Either String Message)

    sendAll s $ "you said: " <> bytes
    talk s model

run_agent :: MaxTurns -> ApiKey -> Model -> IO ()
run_agent 0 _ _ = pure ()
run_agent n key@(ApiKey apiKey) model = do
  let ApiDefinition (BaseUrl baseUrl) (Uri uri) = toApiDefinition model

  runReq defaultHttpConfig $ do
    let payload = ChatPayload
          { messages = [Message User "What is 2+2?"]
          , model    = model
          }

    res <- req POST (https baseUrl /: uri) (ReqBodyJson payload) jsonResponse (header "Authorization" ("Bearer " <> B.pack apiKey))

    liftIO $ print (responseBody res :: Value)

  run_agent (n - 1) key model
