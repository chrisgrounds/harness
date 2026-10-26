module Main (main) where

import Console (runConsole)
import Control.Exception (IOException, bracket, catch, displayException)
import Network.Socket
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hIsTerminalDevice, stdin, stdout)
import UI (runUI)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStrLn help
    [] -> runClient "/tmp/harness.sock"
    [path] | take 1 path /= "-" -> runClient path
    _ -> die help
  where
    help = "Usage: hs-tui-client [SOCKET]\nDefault: /tmp/harness.sock\nEnter sends; Alt-Enter inserts a newline; F1 shows shortcuts; Ctrl-C exits."

runClient :: FilePath -> IO ()
runClient path = catch session reportError
  where
    reportError :: IOException -> IO ()
    reportError err = die ("hs-tui-client: " ++ displayException err)
    session = bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
      connect sock (SockAddrUnix path)
      terminalIn <- hIsTerminalDevice stdin
      terminalOut <- hIsTerminalDevice stdout
      if terminalIn && terminalOut then runUI path sock else runConsole sock
