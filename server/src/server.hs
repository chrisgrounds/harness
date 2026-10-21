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
import GHC.Generics (Generic)
import Data.Text

newtype BaseUrl = BaseUrl Text
  deriving (Show)

newtype Uri = Uri Text
  deriving (Show)

newtype ApiKey = ApiKey String
  deriving (Show)

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

data APIDefinition = APIDefinition BaseUrl Uri
 deriving (Show)

toApiDefinition :: Model -> APIDefinition
toApiDefinition Mercury = APIDefinition (BaseUrl "api.inceptionlabs.ai") (Uri "/v1/chat/completions")

main :: IO ()
main = do
  loadEnv

  let model = Mercury

  apiKey <- ApiKey <$> getEnv "INCEPTION_API_KEY"

  let socketPath = "/tmp/harness.sock"

  sock <- socket AF_UNIX Stream defaultProtocol

  bind sock (SockAddrUnix socketPath)

  harness (MaxTurns 1) apiKey model

  close sock
  removeFile socketPath

newtype MaxTurns = MaxTurns Int
  deriving (Show, Num, Eq)

harness :: MaxTurns -> ApiKey -> Model -> IO ()
harness 0 _ _ = pure ()
harness n key@(ApiKey apiKey) model = do
  let APIDefinition (BaseUrl baseUrl) (Uri uri) = toApiDefinition model

  runReq defaultHttpConfig $ do
    let payload = ChatPayload
          { messages = [Message "user" "What is 2+2?"]
          , model    = "mercury-3"
          }

    res <- req POST (https baseUrl /: uri) (ReqBodyJson payload) jsonResponse (header "Authorization" ("Bearer " <> B.pack apiKey))

    liftIO $ print (responseBody res :: Value)

  harness (n - 1) key model
