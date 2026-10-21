{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Socket
import Network.HTTP.Req
import System.Environment
import LoadEnv
import System.Directory

data AIClient = AIClient {
  baseUrl :: BaseUrl,
  apiKey :: ApiKey
} deriving (Show)

newtype BaseUrl = BaseUrl String
  deriving (Show)

newtype ApiKey = ApiKey String
  deriving (Show)

main :: IO ()
main = do
  loadEnv

  apiKey <- ApiKey <$> getEnv "INCEPTION_API_KEY"
  apiUrl <- BaseUrl <$> getEnv "API_URL"

  let socketPath = "/tmp/harness.sock"

  sock <- socket AF_UNIX Stream defaultProtocol

  bind sock (SockAddrUnix socketPath)

  harness (MaxIterations 5)

  close sock
  removeFile socketPath

newtype MaxIterations = MaxIterations Int
  deriving (Show, Num, Eq)

harness :: MaxIterations -> IO ()
harness 0 = pure ()
harness n = do
  print $ "iteration: " ++ show n

  harness $ n - 1

-- curl -X POST https://api.inceptionlabs.ai/v1/chat/completions \
--   -H "Authorization: Bearer INCEPTION_API_KEY" \
--   -H "Content-Type: application/json" \
--   -d '{"messages": [{"role": "user", "content": "What is a diffusion model?"}], "model": "mercury-3"}'
