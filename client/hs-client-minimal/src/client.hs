{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import Control.Monad (forever)
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO

withSocket :: FilePath -> (Socket -> IO b) -> IO b
withSocket path io =
  bracket (socket AF_UNIX Stream defaultProtocol) close io

main :: IO ()
main = do
  let path = "/tmp/harness.sock"

  withSocket path $
    \s -> do
      connect s (SockAddrUnix path)

      forever $ do
        z <- getLine
        sendAll s $ L.toStrict (encode (object ["role" .= ("user" :: String), "content" .= z]))
        print =<< recv s 1024
