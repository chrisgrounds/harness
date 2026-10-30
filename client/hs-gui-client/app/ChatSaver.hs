-- | Saves the project's chats in a background thread: at once after a
-- change worth keeping straight away, and otherwise once edits and
-- progress have been quiet for 'debounce'.
module ChatSaver (ChatSaver, withChatSaver, offer, updatedTimes) where

import Client.Chats (SavedChat (..), restamp, saveChats, urgentChange)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import Control.Exception (IOException, bracket, catch, displayException, finally)
import Control.Monad (forever, unless, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)
import System.IO (hPutStrLn, stderr)

data ChatSaver = ChatSaver
  { saverFile :: FilePath,
    saverProject :: FilePath,
    -- | The chats as last offered, with their times. Only the frame thread
    -- touches it.
    latest :: IORef [SavedChat],
    -- | Whether anything was offered, so leaving saves once more.
    dirty :: IORef Bool,
    -- | The chats to save next, and whether they are to be saved at once.
    pending :: TVar (Maybe (Bool, [SavedChat]))
  }

-- | Microseconds a change that can wait is held for more changes.
debounce :: Int
debounce = 2000000

-- | Runs the action with a saver whose chats start as given, and saves
-- the latest once more when it ends.
withChatSaver :: FilePath -> FilePath -> [SavedChat] -> (ChatSaver -> IO a) -> IO a
withChatSaver file project initial use = do
  saver <- ChatSaver file project <$> newIORef initial <*> newIORef False <*> newTVarIO Nothing
  let leave = do
        changed <- readIORef (dirty saver)
        when changed (write saver =<< readIORef (latest saver))
  (bracket (forkIO (saving saver)) killThread (const (use saver))) `finally` leave

-- | Hands the saver the chats as they are now, stamped with now: chats
-- unchanged since they were last offered keep their earlier time.
offer :: ChatSaver -> [SavedChat] -> IO ()
offer saver chats = do
  before <- readIORef (latest saver)
  let stamped = restamp before chats
  when (stamped /= before) $ do
    writeIORef (latest saver) stamped
    writeIORef (dirty saver) True
    atomically $ modifyTVar' (pending saver) $ \waiting ->
      Just (maybe False fst waiting || urgentChange before stamped, stamped)

-- | When each chat last changed, as of the last offer.
updatedTimes :: ChatSaver -> IO (Map.Map Int UTCTime)
updatedTimes saver = Map.fromList . map (\c -> (chatId c, lastUpdated c)) <$> readIORef (latest saver)

saving :: ChatSaver -> IO ()
saving saver = forever $ do
  urgent <- atomically (readTVar (pending saver) >>= maybe retry (pure . fst))
  unless urgent $ do
    timer <- registerDelay debounce
    atomically $ (readTVar timer >>= check) `orElse` (readTVar (pending saver) >>= check . maybe False fst)
  chats <- atomically $ do
    waiting <- readTVar (pending saver)
    writeTVar (pending saver) Nothing
    maybe retry (pure . snd) waiting
  write saver chats

write :: ChatSaver -> [SavedChat] -> IO ()
write saver chats = saveChats (saverFile saver) (saverProject saver) chats `catch` report
  where
    report :: IOException -> IO ()
    report err = hPutStrLn stderr ("hs-gui-client: could not save chats: " ++ displayException err)
