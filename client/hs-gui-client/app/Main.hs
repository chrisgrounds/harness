module Main (main) where

import Control.Exception (IOException, bracket, catch, displayException)
import Network.Socket
import System.Directory (doesDirectoryExist, getCurrentDirectory, makeAbsolute)
import System.Environment (getArgs)
import System.Exit (die)
import UI (runUI)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStrLn help
    _ -> case parse "/tmp/harness.sock" Nothing args of
      Nothing -> die help
      Just (path, project) -> do
        dir <- makeAbsolute =<< maybe getCurrentDirectory pure project
        exists <- doesDirectoryExist dir
        if exists then runClient path dir else die ("hs-gui-client: no such directory: " ++ dir)
  where
    parse path _ ("--project" : dir : rest) = parse path (Just dir) rest
    parse _ project (path : rest) | take 1 path /= "-" = parse path project rest
    parse path project [] = Just (path, project)
    parse _ _ _ = Nothing
    help = "Usage: hs-gui-client [--project DIR] [SOCKET]\nDIR is the cabal project runs work on. Default: the current directory\nSOCKET default: /tmp/harness.sock\nEnter sends; Shift-Enter inserts a newline; closing the window exits."

runClient :: FilePath -> FilePath -> IO ()
runClient path dir = catch session reportError
  where
    reportError :: IOException -> IO ()
    reportError err = die ("hs-gui-client: " ++ displayException err)
    session = bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
      connect sock (SockAddrUnix path)
      runUI path dir sock
