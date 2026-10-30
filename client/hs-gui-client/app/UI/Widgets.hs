{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Controls the interface is built from, beyond Dear ImGui's own.
module UI.Widgets
  ( meter,
    sendButton,
    Navigation (..),
    navigationButton,
    resizeSidebar,
    closableModal,
    modelCombo,
  )
where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Text (Text)
import DearImGui hiding (end, flags, indent, size, w, x, y, z)
import qualified DearImGui.Raw as Raw
import qualified DearImGui.Raw.DrawList as DrawList
import Foreign (with, (.|.))
import Foreign.C.Types (CFloat)
import ImGui.Extras (beginClosableModal, beginComboWithFlags, handCursor, setMouseCursor, styleColourU32, withStyleFloat)
import Theme.Palette (Colour)
import UI.Draw (colour, line, stroke)
import UI.Style (transparent, withStyleColours, withStyleVec)
import UI.Text (withText)

-- | A bar of the given width filled to the fraction, from the track's
-- colour to the fill's.
meter :: Colour -> Colour -> Float -> Float -> IO ()
meter track fill width fraction = do
  let height = 4
  with (ImVec2 width height) Raw.dummy
  ImVec2 x0 y0 <- Raw.getItemRectMin
  list <- Raw.getWindowDrawList
  let bar c w = when (w > 0) $ do
        ink <- colour 1 c
        with (ImVec2 x0 y0) $ \from ->
          with (ImVec2 (x0 + w) (y0 + height)) $ \to ->
            DrawList.addRectFilled list from to ink (realToFrac (height / 2)) ImDrawFlags_None
  bar track width
  -- Never thinner than its rounded ends, so it starts as a dot.
  bar fill (if fraction > 0 then max height (width * min 1 fraction) else 0)

-- | A round button of the given diameter with an arrow pointing up, filled
-- with the first colour when enabled, the second when hovered, and the
-- third when disabled. Returns whether it was clicked.
sendButton :: Float -> Bool -> Colour -> Colour -> Colour -> Colour -> IO Bool
sendButton diameter enabled fill hover off arrow = do
  clicked <- with (ImVec2 diameter diameter) $ \size' -> withText "##send" $ \name -> Raw.invisibleButton name size' ImGuiButtonFlags_None
  hovered <- Raw.isItemHovered
  when (enabled && hovered) handCursor
  ImVec2 x0 y0 <- Raw.getItemRectMin
  list <- Raw.getWindowDrawList
  disc <- colour 1 (if not enabled then off else if hovered then hover else fill)
  ink <- colour (if enabled then 1 else 0.45) arrow
  let r = diameter / 2
      (cx, cy) = (x0 + r, y0 + r)
      s = diameter * 0.2
      thickness = realToFrac (max 1.8 (diameter / 16)) :: CFloat
      at dx dy = ImVec2 (cx + dx) (cy + dy)
      path = stroke list ink thickness ImDrawFlags_None
  with (at 0 0) $ \centre -> DrawList.addCircleFilled list centre (realToFrac r) disc 0
  path [at 0 (s * 1.1), at 0 (-s * 1.1)]
  path [at (-s) (-s * 0.1), at 0 (-s * 1.1), at s (-s * 0.1)]
  pure (enabled && clicked)

-- | Navigation controls have semantic names, independent of their artwork.
data Navigation = NewChatButton | SettingsButton
  deriving (Eq, Show)

-- | A compact button with a generous hit target and a drawn icon.
navigationButton :: Navigation -> IO Bool
navigationButton kind = do
  h <- getTextLineHeight
  pressed <- withStyleVec ImGuiStyleVar_FramePadding (ImVec2 17 ((diameter - h) / 2)) $
    withStyleFloat ImGuiStyleVar_FrameBorderSize 0 $
      withStyleColours [(ImGuiCol_Button, transparent)] $
        button name
  ImVec2 x0 y0 <- Raw.getItemRectMin
  list <- Raw.getWindowDrawList
  ink <- styleColourU32 ImGuiCol_Text
  let cx = x0 + diameter / 2
      cy = y0 + diameter / 2
      scale = 0.70
      at px py = ImVec2 (cx + px * scale) (cy + py * scale)
      path = stroke list ink 1.3
  case kind of
    SettingsButton -> do
      let tooth i =
            let angle = fromIntegral i * 2 * pi / 48
                radius = if i `mod` 6 `elem` [1, 2, 3] then 9 else 6.8
            in at (cos angle * radius) (sin angle * radius)
      path ImDrawFlags_Closed (map tooth [0 :: Int .. 47])
      with (at 0 0) $ \centre -> DrawList.addCircle list centre (realToFrac (3 * scale)) ink 0 1.3
    NewChatButton -> do
      path ImDrawFlags_None [at 1 (-7), at (-7) (-7), at (-7) 7, at 7 7, at 7 1]
      path ImDrawFlags_Closed [at (-2) 1, at 6 (-7), at 9 (-4), at 1 4, at (-3) 5]
  hovered <- Raw.isItemHovered
  when hovered $ do
    handCursor
    visible <- Raw.beginTooltip
    when visible $ withText hint (\hintText -> Raw.textUnformatted hintText Nothing) `finally` Raw.endTooltip
  pure pressed
  where
    diameter = 34
    (name, hint) = case kind of
      NewChatButton -> ("##new-chat", "New chat")
      SettingsButton -> ("##user-settings", "User settings")

-- | The divider's width follows horizontal dragging and stays within its bounds.
resizeSidebar :: Float -> Float -> Float -> Float -> IO Float
resizeSidebar width minimum' maximum' height = do
  _ <- invisibleButton "##sidebar-resize" (ImVec2 8 height) ImGuiButtonFlags_None
  active <- Raw.isItemActive
  hovered <- Raw.isItemHovered
  when (active || hovered) (setMouseCursor ImGuiMouseCursor_ResizeEW)
  if not active then pure width else do
    ImVec2 dx _ <- Raw.getMouseDragDelta ImGuiMouseButton_Left 0
    Raw.resetMouseDragDelta ImGuiMouseButton_Left
    pure (max minimum' (min maximum' (width + dx)))

-- | Begin a compact modal with a close button. The caller ends its popup when visible.
closableModal :: Text -> ImVec2 -> IO (Bool, Bool)
closableModal title (ImVec2 width height) = do
  with (ImVec2 280 0) $ \minimum' -> with (ImVec2 (width - 32) (height - 40)) $ \maximum' ->
    Raw.setNextWindowSizeConstraints minimum' maximum'
  withText title $ \name -> beginClosableModal name flags'
  where
    flags' = ImGuiWindowFlags_NoSavedSettings .|. ImGuiWindowFlags_NoCollapse .|. ImGuiWindowFlags_NoResize .|. ImGuiWindowFlags_AlwaysAutoResize

-- | A quiet model picker with a chevron. The caller ends its combo when open.
modelCombo :: Text -> Text -> Float -> IO Bool
modelCombo name caption width = do
  setNextItemWidth width
  opened <- withStyleFloat ImGuiStyleVar_FrameBorderSize 0 $
    withStyleColours [(ImGuiCol_FrameBg, transparent)] $
      withText name $ \label' -> withText caption $ \preview ->
        beginComboWithFlags label' preview (ImGuiComboFlags_NoArrowButton .|. ImGuiComboFlags_HeightLarge)
  when (not opened) $ do
    ImVec2 x0 y0 <- Raw.getItemRectMin
    -- dear-imgui 2.4.1's getItemRectMax calls GetItemRectMin by mistake.
    -- Derive the far corner from the correctly bound item size instead.
    ImVec2 itemWidth itemHeight <- Raw.getItemRectSize
    let edge = x0 + itemWidth
        cy = y0 + itemHeight / 2
    ink <- styleColourU32 ImGuiCol_TextDisabled
    line ink 1.3 (ImVec2 (edge - 13) (cy - 2)) (ImVec2 (edge - 9) (cy + 2))
    line ink 1.3 (ImVec2 (edge - 9) (cy + 2)) (ImVec2 (edge - 5) (cy - 2))
  pure opened
