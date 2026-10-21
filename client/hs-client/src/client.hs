{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import System.IO
import qualified Data.ByteString.Char8 as B
import Control.Monad (forever)
import Control.Exception

main :: IO ()
main = do
  bracket
    (socket AF_UNIX Stream defaultProtocol)
    close
    $ \s -> do
      connect s (SockAddrUnix "/tmp/harness.sock")

      forever $ do
        z <- getLine
        sendAll s $ B.pack z
        print =<< recv s 1024
