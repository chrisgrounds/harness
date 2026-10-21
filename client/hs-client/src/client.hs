{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import System.IO
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  let socketPath = "/tmp/harness.sock"
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix socketPath)
  z <- getLine
  sendAll sock $ B.pack z
  print =<< recv sock 1024
  close sock
