{-# LANGUAGE OverloadedStrings #-}

module UI.Composer
  ( ComposerState, ComposerView (..), ComposerLayout, newComposerState,
    measureComposer, reservedHeight, drawComposer
  ) where

import Client (safeText)
import Client.Settings
import Control.Exception (bracket_, finally)
import Control.Monad (forM, forM_)
import Core (modelName, stepLabel, provider, Provider (OpenAI), ReasoningEffort (..), reasoningEffortName)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import DearImGui hiding (end, flags, indent, size, w, x, y, z)
import qualified DearImGui.Raw as Raw
import qualified DearImGui.Raw.DrawList as DrawList
import Foreign (nullPtr, with)
import ImGui.Extras (Metrics (..), withStyleFloat)
import Theme (metrics, mix, raised, seam)
import Theme.Palette
import UI.Appearance (Appearance)
import qualified UI.Appearance as Appearance
import UI.Composer.Input (Input, input, newInput)
import UI.Draw (colour)
import UI.Fonts
import UI.Icons (Icon (..), icon)
import UI.Layout (gap, setCursorPos')
import UI.Motion
import UI.State (ChatAction (..), Status (..))
import UI.Style (transparent, withStyleColours, withStyleVec)
import UI.Text (ellipsize, label, textWidth, withText)
import UI.Widgets (modelCombo, sendButton)

data ComposerState = ComposerState
  { appearance :: Appearance,
    motion :: Motion,
    composerText :: Input,
    composing :: IORef Bool
  }

data ComposerView = ComposerView
  { composerDraft :: Text,
    composerReplacement :: Maybe Text,
    composerCanSend :: Bool,
    composerFailure :: Maybe Text,
    composerBlocked :: Bool,
    composerWaiting :: Bool,
    composerChoices :: [ModelChoice],
    composerChoice :: Maybe ModelChoice,
    composerEffort :: Maybe ReasoningEffort
  }

data ComposerLayout = ComposerLayout
  { boxHeight :: Float,
    reservedHeight :: Float
  }

newComposerState :: Appearance -> Motion -> IO ComposerState
newComposerState look moves = ComposerState look moves <$> newInput <*> newIORef False

palette :: ComposerState -> Palette
palette = Appearance.palette . appearance

fonts :: ComposerState -> Fonts
fonts = Appearance.fonts . appearance

drawComposer :: ComposerState -> ComposerView -> ComposerLayout -> Status -> Float -> Float -> IO [ChatAction]
drawComposer e view layout currentStatus left column = do
  case currentStatus of
    Ready -> pure ()
    _ -> do
      gap statusGap
      setCursorPosX left
      statusLine e currentStatus left column
      gap composerGap
  composerRow e view layout left column

-- | What the latest run is doing, above the composer.
statusLine :: ComposerState -> Status -> Float -> Float -> IO ()
statusLine e currentStatus left column = do
  case currentStatus of
    Offline _ -> icon Cross (danger p) >> label (danger p) "Disconnected"
    Busy doing detail -> do
      _ <- icon Spinner (accent p)
      label (accent p) doing
      forM_ detail $ \call -> do
        Raw.sameLine
        here <- getCursorPosX
        bodyHeight <- getTextLineHeight
        withFont (small (fonts e)) $ do
          shown <- ellipsize (left + column - here - 8) (safeText call)
          -- Lines the smaller text up with the body's baseline.
          smallHeight <- getTextLineHeight
          setCursorPosY . (+ max 0 ((bodyHeight - smallHeight) * 0.8)) =<< getCursorPosY
          label (muted p) shown
    Stopped step -> icon Cross (danger p) >> label (danger p) ("Stopped at " <> T.toLower (stepLabel step))
    Ready -> pure ()
  where
    p = palette e

-- | One rounded composer, with model and effort pickers and an up arrow in its footer.
composerRow :: ComposerState -> ComposerView -> ComposerLayout -> Float -> Float -> IO [ChatAction]
composerRow e view layout left column = do
  let height = boxHeight layout
  wasActive <- readIORef (composing e)
  focusGlow <- ease (motion e) ComposerFocus 0.1 (if wasActive then 1 else 0)
  -- The edge lightens while typing; orange is kept for what is running.
  let edge = mix (realToFrac (0.22 * focusGlow)) (foreground p) (seam p)
      textHeight = height - 2 * composerInset - composerControlsHeight - 8
  setCursorPosX left
  ImVec2 _ top <- Raw.getCursorPos
  panel (ImVec2 column height) (raised p) edge
  setCursorPos' (ImVec2 (left + 4) (top + 4))
  -- Keep the keyboard in the composer unless something else is in use.
  popup <- isAnyLevelPopupOpen ""
  activeItem <- Raw.isAnyItemActive
  focusedItem <- Raw.isAnyItemFocused
  let focus = not (composerBlocked view || popup || activeItem || focusedItem)
  (submitted, replaced, value) <-
    withStyleVec ImGuiStyleVar_FramePadding (ImVec2 12 (snd (framePadding metrics))) $
      withStyleFloat ImGuiStyleVar_FrameBorderSize 0 $
        withStyleColours [(col, transparent) | col <- [ImGuiCol_FrameBg, ImGuiCol_FrameBgHovered, ImGuiCol_FrameBgActive]] $
          input (composerText e) hint (ImVec2 (column - 8) textHeight) focus (composerReplacement view)
  writeIORef (composing e) =<< Raw.isItemActive
  let controlsTop = top + height - composerInset - composerControlsHeight
  pickerHeight <- withFont (small (fonts e)) $ (+ 2 * snd (framePadding metrics)) <$> getTextLineHeight
  setCursorPosY (controlsTop + (composerControlsHeight - pickerHeight) / 2)
  modelActions <- modelPicker e view (left + composerInset) (column - 2 * composerInset - composerControlsHeight - 8)
  setCursorPos' (ImVec2 (left + column - composerInset - composerControlsHeight) controlsTop)
  clicked <- sendButton composerControlsHeight (composerCanSend view) (accent p) (secondaryAccent p) (seam p) (foreground p)
  setCursorPos' (ImVec2 left top)
  with (ImVec2 column height) Raw.dummy
  pure (modelActions ++ [Replaced expected | replaced, Just expected <- [composerReplacement view]] ++ [Edited value | value /= composerDraft view] ++ [Send | submitted || clicked])
  where
    p = palette e
    hint = case composerFailure view of
      Just _ -> "Disconnected from the server"
      Nothing -> "Message…"

-- | The rounded surface behind the text and the controls. Leaves the cursor
-- in place so both can be laid out inside it.
panel :: ImVec2 -> Colour -> Colour -> IO ()
panel (ImVec2 width height) fill edge = do
  ImVec2 x0 y0 <- Raw.getCursorScreenPos
  list <- Raw.getWindowDrawList
  surface <- colour 1 fill
  outline <- colour 1 edge
  with (ImVec2 x0 y0) $ \from ->
    with (ImVec2 (x0 + width) (y0 + height)) $ \to -> do
      DrawList.addRectFilled list from to surface 16 ImDrawFlags_None
      DrawList.addRect list from to outline 16 1 ImDrawFlags_None

composerInset, composerControlsHeight :: Float
composerInset = 12
composerControlsHeight = 32

-- | Measure once before reserving the transcript's space; drawing consumes
-- the same layout without advancing its height animation again.
measureComposer :: ComposerState -> ComposerView -> Status -> Float -> IO ComposerLayout
measureComposer e view currentStatus column = do
  lineHeight <- getTextLineHeight
  -- InputTextMultiline reserves room for its scrollbar even before it appears.
  let wrapWidth = max 1 (column - 8 - 2 * 12 - scrollbarSize metrics)
      value = fromMaybe (composerDraft view) (composerReplacement view)
  ImVec2 _ textHeight <- withText value $ \textPtr -> Raw.calcTextSize textPtr nullPtr 0 (realToFrac wrapWidth)
  let trailingLine = if "\n" `T.isSuffixOf` value then 1 else 0
      rows = max 2 (min 6 ((ceiling (textHeight / lineHeight) :: Int) + trailingLine))
  height <- ease (motion e) ComposerHeight 0.07
    (fromIntegral rows * lineHeight + 2 * snd (framePadding metrics) + 2 * composerInset + composerControlsHeight + 8)
  let statusHeight = case currentStatus of Ready -> 0; _ -> statusGap + lineHeight + composerGap + snd (itemSpacing metrics)
  pure (ComposerLayout height (statusHeight + height + snd (itemSpacing metrics) + bottomGap))

statusGap, composerGap, bottomGap :: Float
statusGap = 4
composerGap = 6
bottomGap = 36

-- | The selected model is scoped to this chat, including before its first run.
modelPicker :: ComposerState -> ComposerView -> Float -> Float -> IO [ChatAction]
modelPicker e view left column = withFont (small (fonts e)) $ do
  let preview = maybe "Choose a model" (fst . T.breakOn " · " . choiceLabel) (composerChoice view)
      showEffort = maybe False ((== OpenAI) . provider . choiceModel) (composerChoice view)
      effortPreview = "Effort: " <> effortLabel (composerEffort view)
  measured <- textWidth preview
  effortMeasured <- textWidth effortPreview
  let effortWidth = if showEffort then min (column / 2) (effortMeasured + 40) else 0
      effortSpace = if showEffort then effortWidth + 8 else 0
      width = min (column - effortSpace) (measured + 40)
      controlsLeft = left + column - width - effortSpace
  shown <- ellipsize (width - 40) preview
  setCursorPosX controlsLeft
  bracket_ (beginDisabled (composerWaiting view)) Raw.endDisabled $ do
    opened <- modelCombo "##chat-model" shown width
    modelActions <- if not opened then pure [] else flip finally Raw.endCombo $
      concat <$> forM (composerChoices view) (\choice -> do
        picked <- selectableWith defSelectableOptions {selected = Just choice == composerChoice view}
          (fst (T.breakOn " · " (choiceLabel choice)) <> "##" <> modelName (choiceModel choice) <> fromMaybe "" (choiceAccount choice))
        pure [ChooseModel choice | picked])
    effortActions <- if not showEffort then pure [] else do
      Raw.sameLine
      effortShown <- ellipsize (effortWidth - 40) effortPreview
      effortOpened <- modelCombo "##chat-effort" effortShown effortWidth
      if not effortOpened then pure [] else flip finally Raw.endCombo $
        concat <$> forM (Nothing : map Just [minBound .. maxBound]) (\effort -> do
          picked <- selectableWith defSelectableOptions {selected = effort == composerEffort view} (effortLabel effort)
          pure [ChooseEffort effort | picked])
    pure (modelActions ++ effortActions)
  where
    effortLabel Nothing = "Default"
    effortLabel (Just EffortXHigh) = "Extra high"
    effortLabel (Just effort) = T.toTitle (reasoningEffortName effort)
