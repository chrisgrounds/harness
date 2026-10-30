{-# LANGUAGE OverloadedStrings #-}

module UI.Transcript (Transcript, TranscriptView (..), newTranscript, drawTranscript) where

import Client (safeText)
import Control.Monad (forM, forM_, unless, void, when, zipWithM)
import Core
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import DearImGui hiding (end, flags, indent, size, w, x, y, z)
import qualified DearImGui.Raw as Raw
import ImGui.Extras (Metrics (..), scrollY, scrollMaxY, setScrollY, withAlpha)
import Theme (metrics, seam)
import Theme.Palette
import UI.Appearance (Appearance)
import qualified UI.Appearance as Appearance
import UI.Fonts
import UI.Draw (colour, line)
import UI.Icons (Icon (..), icon, iconWidth)
import UI.Layout (gap, measure)
import UI.Motion
import UI.State (ChatAction (..), Scroll (..), implementCounts, target)
import UI.Text (label, paragraph, rightAt)
import UI.Widgets (meter)

data Transcript = Transcript
  { appearance :: Appearance,
    motion :: Motion,
    scrolling :: IORef Scrolling,
    runTops :: IORef (Map.Map Int Float),
    selectedChat :: IORef (Maybe Int)
  }

data TranscriptView = TranscriptView
  { transcriptChat :: Int,
    -- | Newest first. The oldest-first ordinal is stable as runs append;
    -- together with the chat ID it identifies each run for this session.
    transcriptRuns :: [Run],
    transcriptScroll :: Maybe Scroll
  }

data Scrolling = Scrolling
  { following :: Bool,
    destination :: Maybe Float,
    lastSet :: Maybe Float
  }

newTranscript :: Appearance -> Motion -> IO Transcript
newTranscript look moves = Transcript look moves
  <$> newIORef (Scrolling True Nothing Nothing)
  <*> newIORef Map.empty
  <*> newIORef Nothing

palette :: Transcript -> Palette
palette = Appearance.palette . appearance

fonts :: Transcript -> Fonts
fonts = Appearance.fonts . appearance

-- | Draw inside the transcript pane. All mutable scroll bookkeeping stays
-- here, including resetting it when the selected conversation changes.
drawTranscript :: Transcript -> TranscriptView -> Float -> Float -> IO [ChatAction]
drawTranscript e view left column = do
  previous <- readIORef (selectedChat e)
  when (previous /= Just (transcriptChat view)) $ do
    writeIORef (scrolling e) (Scrolling True Nothing Nothing)
    writeIORef (runTops e) Map.empty
    writeIORef (selectedChat e) (Just (transcriptChat view))
  if null (transcriptRuns view)
    then emptyTranscript e left column
    else void (zipWithM (drawRun e (transcriptChat view) left column) [0 ..] (reverse (transcriptRuns view)))
  gap 28
  follow e (transcriptScroll view)

-- | What an empty transcript asks.
emptyTranscript :: Transcript -> Float -> Float -> IO ()
emptyTranscript e left column = do
  ImVec2 _ avail <- Raw.getContentRegionAvail
  gap (max 40 (avail * 0.28))
  setCursorPosX left
  withFont (heading (fonts e)) $ paragraph (measure left column) (foreground p) "Describe what to build."
  gap 6
  setCursorPosX left
  paragraph (measure left column) (muted p) "Each message starts a run: types, functions, properties,\nthen parallel implementation, type checking and property tests."
  where
    p = palette e

-- | Glides to a requested scroll, or keeps the latest output in view while
-- the transcript is at the bottom. Scrolling by hand stops following, and
-- scrolling back to the bottom starts again.
follow :: Transcript -> Maybe Scroll -> IO [ChatAction]
follow e request = do
  y <- scrollY
  bottom <- scrollMaxY
  page <- getWindowHeight
  tops <- readIORef (runTops e)
  s <- readIORef (scrolling e)
  dt <- elapsed (motion e)
  let movedByHand = maybe False (\l -> abs (y - l) > 1) (lastSet s)
      from = fromMaybe y (destination s)
      requested = case request of
        Just ToFirst -> Just (Just 0)
        Just ToLatest -> Just Nothing
        Just PageUp -> Just (Just (from - page * 0.85))
        Just PageDown -> Just (Just (from + page * 0.85))
        Just (ToRun i) -> Just (subtract 24 <$> Map.lookup i tops)
        Nothing -> Nothing
      (following', destination') = case requested of
        Just Nothing -> (True, Nothing)
        Just (Just d) -> (False, Just (clamp d))
        Nothing
          | movedByHand -> (y >= bottom - 1, Nothing)
          | otherwise -> (following s, destination s)
      clamp = max 0 . min bottom
      goal = if following' then Just bottom else destination'
  case goal of
    Just d -> do
      let next = d + (y - d) * exp (negate dt / 0.075)
          arrived = abs (next - d) < 0.5
          set = if arrived then d else next
      unless arrived (keepMoving (motion e))
      setScrollY set
      writeIORef (scrolling e) (Scrolling following' (if arrived then Nothing else destination') (Just set))
    Nothing -> writeIORef (scrolling e) (Scrolling following' Nothing Nothing)
  pure [Scrolled | isJust (request)]

-- | A run: the requirement, then its steps on a rail that fills as they
-- pass, then why it looped back, any warnings and the files it changed.
drawRun :: Transcript -> Int -> Float -> Float -> Int -> Run -> IO ()
drawRun e chat left column index run = do
  ImVec2 _ top <- Raw.getCursorPos
  modifyIORef' (runTops e) (Map.insert index top)
  -- A new run fades in as it is sent.
  appear <- easeFrom (motion e) (RunAppearance chat index) 0.09 0 1
  withAlpha appear $ do
    when (index > 0) $ do
      gap 30
      rule <- colour 1 (seam p)
      ImVec2 _ ruleY <- Raw.getCursorScreenPos
      ImVec2 windowX _ <- Raw.getWindowPos
      line rule 1 (ImVec2 (windowX + left) ruleY) (ImVec2 (windowX + left + column) ruleY)
      gap 30
    when (index == 0) (gap 30)
    requirementBlock e left column (safeText (requirementText (requirement run)))
    gap 16
    setCursorPosX left
    forM_ (target run) $ \t -> withFont (mono (fonts e)) $ label (muted p) (safeText t)
    when (isJust (target run) && iteration run > 1) $ rightAt (left + column) (muted p) ("Iteration " <> T.pack (show (iteration run)))
    gap 10
    nodes <- forM (steps run) (drawStep e chat index left column run)
    rail e chat index nodes
    unless (null (history run)) $ do
      gap 12
      forM_ (history run) $ \note -> do
        setCursorPosX left
        -- Loop-backs name their iteration; the review's conclusion does not.
        _ <- icon (if "Iteration" `T.isPrefixOf` note then Retry else Dot) (muted p)
        paragraph (measure left column) (muted p) (safeText note)
    forM_ (warnings run) $ \warning -> do
      gap 10
      setCursorPosX left
      _ <- icon Caution (danger p)
      paragraph (measure left column) (danger p) (safeText warning)
    unless (null (touchedFiles run)) $ do
      gap 14
      withFont (mono (fonts e)) $
        forM_ (touchedFiles run) $ \file -> do
          setCursorPosX left
          label (if changedKind file == NewFile then success p else secondaryAccent p) (safeText (changeTag file))
  where
    p = palette e

-- | The user's words, set a weight up beside a bar in their colour.
requirementBlock :: Transcript -> Float -> Float -> Text -> IO ()
requirementBlock e left column words' = do
  setCursorPosX (left + 16)
  ImVec2 x0 y0 <- Raw.getCursorScreenPos
  withFont (lead (fonts e)) $ paragraph (measure left column) (foreground p) words'
  ImVec2 _ below <- Raw.getCursorScreenPos
  bar <- colour 1 (highlight p)
  line bar 3 (ImVec2 (x0 - 16) (y0 + 3)) (ImVec2 (x0 - 16) (below - snd (itemSpacing metrics) - 3))
  where
    p = palette e

-- | A step on the rail, with its failure or, while functions are being
-- implemented, their progress. Returns its icon's centre and radius, and
-- whether it passed.
drawStep :: Transcript -> Int -> Int -> Float -> Float -> Run -> (Step, StepStatus) -> IO ((ImVec2, Float), Bool)
drawStep e chat index left column run (step, stepStatus) = do
  let (kind, tone) = statusIcon p stepStatus
      counts = implementCounts step run
  setCursorPosX left
  node <- icon kind tone
  label (if stepStatus == Passed then foreground p else tone) (stepLabel step <> maybe "" (\(done, total) -> "  " <> T.pack (show done) <> "/" <> T.pack (show total)) counts)
  indent' <- (left +) <$> iconWidth
  case stepStatus of
    -- Usually the compiler's own words, so set as code.
    Failed reason -> do
      gap 2
      setCursorPosX indent'
      withFont (mono (fonts e)) $ paragraph (left + column) (danger p) (safeText reason)
      gap 4
    Running | step == Implement -> do
      forM_ counts $ \(done, total) -> do
        gap 2
        setCursorPosX indent'
        progress <- ease (motion e) (RunProgress chat index) 0.12 (fromIntegral done / fromIntegral total)
        meter (seam p) (accent p) (min 300 (column - (indent' - left))) progress
        gap 4
      mapM_ (drawTask e indent' (left + column)) (tasks run)
      gap 4
    _ -> pure ()
  -- Room between steps, so the rail reads as one line through them.
  gap 2
  pure (node, stepStatus == Passed)
  where
    p = palette e

-- | The line joining a run's steps: grey, filling with the passed colour
-- down from each step that passes, and draining as a new iteration resets
-- them.
rail :: Transcript -> Int -> Int -> [((ImVec2, Float), Bool)] -> IO ()
rail e chat index nodes = do
  track <- colour 1 (seam p)
  forM_ (zip3 [0 :: Int ..] nodes (drop 1 nodes)) $ \(k, ((ImVec2 cx y0, r0), passed), ((ImVec2 _ y1, r1), _)) -> do
    let top = y0 + r0 + 3
        bottom = y1 - r1 - 3
    when (bottom > top) $ do
      line track 1.5 (ImVec2 cx top) (ImVec2 cx bottom)
      filled <- ease (motion e) (RunRail chat index k) 0.14 (if passed then 1 else 0)
      when (filled > 0.01) $ do
        fill <- colour (0.75 * filled) (success p)
        line fill 1.5 (ImVec2 cx top) (ImVec2 cx (top + (bottom - top) * filled))
  where
    p = palette e

drawTask :: Transcript -> Float -> Float -> (FunctionRef, TaskStatus) -> IO ()
drawTask e indent' edge (ref, taskStatus) = do
  let (kind, tone) = taskIcon p taskStatus
  setCursorPosX indent'
  _ <- icon kind tone
  withFont (mono (fonts e)) $ label (if taskStatus == Queued then muted p else foreground p) (safeText (functionNameText (refName ref)))
  Raw.sameLine
  setCursorPosY . (+ 1) =<< getCursorPosY
  withFont (small (fonts e)) $ label (muted p) (safeText (T.pack (refPath ref)))
  case taskStatus of
    TaskFailed reason -> do
      nested <- (indent' +) <$> iconWidth
      setCursorPosX nested
      paragraph edge (danger p) (safeText reason)
    _ -> pure ()
  where
    p = palette e

statusIcon :: Palette -> StepStatus -> (Icon, Colour)
statusIcon p Pending = (Hollow, muted p)
statusIcon p Running = (Spinner, accent p)
statusIcon p Passed = (Tick, success p)
statusIcon p (Failed _) = (Cross, danger p)

taskIcon :: Palette -> TaskStatus -> (Icon, Colour)
taskIcon p Queued = (Hollow, muted p)
taskIcon p Working = (Spinner, accent p)
taskIcon p Done = (Tick, success p)
taskIcon p (TaskFailed _) = (Cross, danger p)
