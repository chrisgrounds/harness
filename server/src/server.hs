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

data AIClient = AIClient {
  baseUrl :: BaseUrl,
  apiKey :: ApiKey
} deriving (Show)

newtype BaseUrl = BaseUrl String
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

main :: IO ()
main = do
  loadEnv

  apiKey <- ApiKey <$> getEnv "INCEPTION_API_KEY"
  apiUrl <- BaseUrl <$> getEnv "API_URL"

  let socketPath = "/tmp/harness.sock"

  sock <- socket AF_UNIX Stream defaultProtocol

  bind sock (SockAddrUnix socketPath)

  harness (MaxTurns 1) (AIClient apiUrl apiKey)

  close sock
  removeFile socketPath

newtype MaxTurns = MaxTurns Int
  deriving (Show, Num, Eq)

harness :: MaxTurns -> AIClient -> IO ()
harness 0 _ = pure ()
harness n aiClient = do
  print $ "iteration: " ++ show n

  runReq defaultHttpConfig $ do
    let payload = ChatPayload
          { messages = [Message "user" "What is a diffusion model?"]
          , model    = "mercury-3"
          }

    let ApiKey rawKey = apiKey aiClient

    res <- req POST (https "api.inceptionlabs.ai" /: "/v1/chat/completions") (ReqBodyJson payload) jsonResponse (header "Authorization" ("Bearer " <> B.pack rawKey))

    liftIO $ print (responseBody res :: Value)

  harness (n - 1) aiClient
