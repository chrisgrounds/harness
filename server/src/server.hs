{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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

newtype BaseUrl = BaseUrl Text
  deriving (Show)

newtype Uri = Uri Text
  deriving (Show)

newtype ApiKey = ApiKey String
  deriving (Show)

newtype MaxTurns = MaxTurns Int
  deriving (Show, Num, Eq)

data Message = Message {
  role :: String,
  content :: String
} deriving (Show, Generic)

data ChatPayload = ChatPayload
  { messages :: [Message]
  , model    :: String
  } deriving (Show, Generic)

instance ToJSON Message
instance ToJSON ChatPayload

data Model = Mercury

data ApiDefinition = ApiDefinition BaseUrl Uri
 deriving (Show)

toApiDefinition :: Model -> ApiDefinition
toApiDefinition Mercury = ApiDefinition (BaseUrl "api.inceptionlabs.ai") (Uri "/v1/chat/completions")

-- TODO:
-- Listen to socket and serialize request into type
-- handle tool calls
-- save history
-- send response to client
-- build a test client

main :: IO ()
main = do
  loadEnv
  apiKey <- ApiKey <$> getEnv "INCEPTION_API_KEY"

  let model = Mercury

  let socketPath = "/tmp/harness.sock"
  sock <- socket AF_UNIX Stream defaultProtocol
  bind sock (SockAddrUnix socketPath)

  listen sock 1

  (conn, _) <- accept sock

  talk conn

  close conn
  -- run_agent (MaxTurns 1) apiKey model

  close sock
  removeFile socketPath

talk :: Socket -> IO ()
talk s = do
  msg <- recv s 1024
  unless (S.null msg) $ do
    sendAll s $ "you said: " <> msg
    talk s

run_agent :: MaxTurns -> ApiKey -> Model -> IO ()
run_agent 0 _ _ = pure ()
run_agent n key@(ApiKey apiKey) model = do
  let ApiDefinition (BaseUrl baseUrl) (Uri uri) = toApiDefinition model

  runReq defaultHttpConfig $ do
    let payload = ChatPayload
          { messages = [Message "user" "What is 2+2?"]
          , model    = "mercury-3"
          }

    res <- req POST (https baseUrl /: uri) (ReqBodyJson payload) jsonResponse (header "Authorization" ("Bearer " <> B.pack apiKey))

    liftIO $ print (responseBody res :: Value)

  run_agent (n - 1) key model
