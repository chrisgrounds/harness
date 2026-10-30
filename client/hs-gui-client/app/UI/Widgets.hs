{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Small Dear ImGui pieces the interface is drawn with.
module UI.Widgets
  ( withText,
    textWidth,
    ellipsize,
    label,
    paragraph,
    rightAligned,
    gap,
    withStyleVec,
    withColourOf,
    colour,
    line,
    Icon (..),
    icon,
    iconWidth,
    meter,
    sendButton,
    Composer,
    newComposer,
    composerPanel,
    composer,
    Navigation (..),
    navigationButton,
    resizeSidebar,
    closableModal,
    modelCombo,
  )
where

import Control.Exception (bracket_, finally)
import Control.Monad (when)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import DearImGui hiding (end, flags, indent, size, w, x, y, z)
import qualified DearImGui.Raw as Raw
import qualified DearImGui.Raw.DrawList as DrawList
import Foreign
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CFloat)
import ImGui.Extras (Metrics (..), colourU32, composerBox, handCursor, styleColourU32, setMouseCursor, getFramePadding, beginClosableModal, beginComboWithFlags, withStyleFloat)
import Theme (metrics, vec4)
import Theme.Palette (Colour (..))

withText :: Text -> (CString -> IO a) -> IO a
withText = B.useAsCString . encodeUtf8

-- | The text's width in the current font.
textWidth :: Text -> IO Float
textWidth t = do
  ImVec2 width _ <- withText t $ \p -> Raw.calcTextSize p nullPtr 0 (-1)
  pure width

-- | The first line of the text, cut short with an ellipsis to fit the width
-- in the current font.
ellipsize :: Float -> Text -> IO Text
ellipsize width t = do
  let first = T.takeWhile (/= '\n') t
      cut = first /= t
  full <- textWidth first
  if full <= width && not cut
    then pure first
    else go first
  where
    go s
      | T.null s = pure "…"
      | otherwise = do
          let shorter = T.stripEnd (T.dropEnd 1 s)
              candidate = shorter <> "…"
          fits <- (<= width) <$> textWidth candidate
          if fits then pure candidate else go shorter

-- | Text drawn in the colour.
withColourOf :: Colour -> IO a -> IO a
withColourOf c = bracket_ (with (vec4 c) (Raw.pushStyleColor ImGuiCol_Text)) (Raw.popStyleColor 1)

-- | Text in a colour, on one line.
label :: Colour -> Text -> IO ()
label c t = withColourOf c (withText t (`Raw.textUnformatted` Nothing))

-- | Text in a colour, wrapped at the given position within the window, or
-- its edge with 0. Every line starts where the first does, so it hangs
-- beside an icon.
paragraph :: Float -> Colour -> Text -> IO ()
paragraph wrapAt c t = bracket_ (Raw.pushTextWrapPos (realToFrac wrapAt)) Raw.popTextWrapPos (label c t)

-- | Text in a colour at the right end of the current line.
rightAligned :: Colour -> Text -> IO ()
rightAligned c t = do
  width <- textWidth t
  Raw.sameLine
  ImVec2 avail _ <- Raw.getContentRegionAvail
  left <- getCursorPosX
  setCursorPosX (left + max 0 (avail - width))
  label c t

-- | Empty space of the given height.
gap :: Float -> IO ()
gap h = with (ImVec2 0 h) Raw.dummy

withStyleVec :: ImGuiStyleVar -> ImVec2 -> IO a -> IO a
withStyleVec var value = bracket_ (with value (Raw.pushStyleVar var)) (Raw.popStyleVar 1)

-- | A colour for draw lists at the given opacity, faded like the items
-- around it.
colour :: Float -> Colour -> IO ImU32
colour alpha (Colour r g b) = colourU32 (f r) (f g) (f b) alpha
  where
    f v = fromIntegral v / 255

-- | A straight line on the window's draw list.
line :: ImU32 -> Float -> ImVec2 -> ImVec2 -> IO ()
line c thickness from to = do
  list <- Raw.getWindowDrawList
  with from $ \a -> with to $ \b -> DrawList.addLine list a b c (realToFrac thickness)

-- | Drawn shapes rather than characters, so they do not depend on the font.
data Icon
  = -- | Not started.
    Hollow
  | -- | In progress; it turns.
    Spinner
  | -- | Ready.
    Dot
  | Tick
  | Cross
  | -- | Looped back.
    Retry
  | Caution

-- | The width an icon takes before the text beside it.
iconWidth :: IO Float
iconWidth = (+ fst (itemSpacing metrics)) <$> getTextLineHeight

-- | An icon in a colour, a line of text high, with the next item beside it.
-- Returns its centre on the screen and its radius.
icon :: Icon -> Colour -> IO (ImVec2, Float)
icon kind c = do
  h <- getTextLineHeight
  with (ImVec2 h h) Raw.dummy
  ImVec2 x0 y0 <- Raw.getItemRectMin
  list <- Raw.getWindowDrawList
  time <- Raw.getTime
  ink <- colour 1 c
  let at fx fy = ImVec2 (x0 + fx * h) (y0 + fy * h)
      r = h * 0.3
      radius = realToFrac r
      thickness = realToFrac (max 1.5 (h / 10))
      stroke points = do
        DrawList.pathClear list
        mapM_ (`with` DrawList.pathLineTo list) points
        DrawList.pathStroke list ink ImDrawFlags_None thickness
      arc from to = do
        DrawList.pathClear list
        with (at 0.5 0.5) $ \centre -> DrawList.pathArcTo list centre radius (realToFrac from) (realToFrac to) 0
        DrawList.pathStroke list ink ImDrawFlags_None thickness
  case kind of
    Hollow -> with (at 0.5 0.5) $ \centre -> DrawList.addCircle list centre radius ink 0 thickness
    Dot -> with (at 0.5 0.5) $ \centre -> DrawList.addCircleFilled list centre (radius * 0.8) ink 0
    Spinner -> let start = realToFrac time * 6 :: Float in arc start (start + 4.4)
    Tick -> stroke [at 0.24 0.53, at 0.42 0.71, at 0.77 0.31]
    Cross -> stroke [at 0.29 0.29, at 0.71 0.71] >> stroke [at 0.71 0.29, at 0.29 0.71]
    Retry -> do
      -- Clockwise from the top, with an arrowhead where it ends.
      let start = -pi / 2 + 0.5 :: Float
          finish = start + 1.6 * pi
          (cx, cy) = (x0 + h / 2, y0 + h / 2)
          (px, py) = (cx + r * cos finish, cy + r * sin finish)
          (tx, ty) = (-(sin finish), cos finish)
          (nx, ny) = (cos finish, sin finish)
          head' = h * 0.16
      arc start finish
      with (ImVec2 (px + tx * head' * 1.4) (py + ty * head' * 1.4)) $ \tip ->
        with (ImVec2 (px + nx * head') (py + ny * head')) $ \outer ->
          with (ImVec2 (px - nx * head') (py - ny * head')) $ \inner ->
            DrawList.addTriangleFilled list tip outer inner ink
    Caution -> do
      stroke [at 0.5 0.16, at 0.88 0.84, at 0.12 0.84, at 0.5 0.16]
      stroke [at 0.5 0.4, at 0.5 0.6]
      with (at 0.5 0.72) $ \dot -> DrawList.addCircleFilled list dot (realToFrac (h * 0.05)) ink 0
  Raw.sameLine
  pure (at 0.5 0.5, r)

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
  with (at 0 0) $ \centre -> DrawList.addCircleFilled list centre (realToFrac r) disc 0
  let stroke points = do
        DrawList.pathClear list
        mapM_ (`with` DrawList.pathLineTo list) points
        DrawList.pathStroke list ink ImDrawFlags_None thickness
  stroke [at 0 (s * 1.1), at 0 (-s * 1.1)]
  stroke [at (-s) (-s * 0.1), at 0 (-s * 1.1), at s (-s * 0.1)]
  pure (enabled && clicked)

-- | The composer's text, which Dear ImGui edits in place.
newtype Composer = Composer (ForeignPtr CChar)

capacity :: Int
capacity = 64 * 1024

newComposer :: IO Composer
newComposer = do
  buffer <- mallocForeignPtrBytes capacity
  withForeignPtr buffer $ \p -> poke p 0
  pure (Composer buffer)

-- | The shared rounded surface behind the text and composer controls.
-- Leaves the cursor in place so both can be laid out inside it.
composerPanel :: ImVec2 -> Colour -> Colour -> IO ()
composerPanel (ImVec2 width height) fill edge = do
  ImVec2 x0 y0 <- Raw.getCursorScreenPos
  list <- Raw.getWindowDrawList
  surface <- colour 1 fill
  outline <- colour 1 edge
  with (ImVec2 x0 y0) $ \from ->
    with (ImVec2 (x0 + width) (y0 + height)) $ \to -> do
      DrawList.addRectFilled list from to surface 16 ImDrawFlags_None
      DrawList.addRect list from to outline 16 ImDrawFlags_None 1

-- | Draws the composer at the given size, showing the hint while it is
-- empty, taking the keyboard when asked, and replacing its text when given
-- a replacement. Returns whether Enter submitted, whether the replacement
-- was made, and the text.
composer :: Composer -> Text -> ImVec2 -> Bool -> Maybe Text -> IO (Bool, Bool, Text)
composer (Composer buffer) hint (ImVec2 width height) focus replacement =
  withForeignPtr buffer $ \p -> do
    let fitted = fitUtf8 (capacity - 1) <$> replacement
    -- Shown at once if the composer is not active; when it is, Dear ImGui
    -- keeps its own copy until the replacement is made.
    mapM_ (write p) fitted
    (submitted, replaced) <-
      B.useAsCString "##composer" $ \name ->
        maybe ($ nullPtr) B.useAsCString fitted $ \replacing ->
          composerBox name p capacity width height focus replacing
    contents <- decodeUtf8With lenientDecode <$> B.packCString p
    when (T.null contents && not (T.null hint)) $ do
      ImVec2 x0 y0 <- Raw.getItemRectMin
      ImVec2 px py <- getFramePadding
      list <- Raw.getWindowDrawList
      ink <- styleColourU32 ImGuiCol_TextDisabled
      with (ImVec2 (x0 + px) (y0 + py)) $ \at ->
        withText hint $ \hintText -> DrawList.addText_ list at ink hintText nullPtr
    pure (submitted, replaced, contents)
  where
    write p bytes = B.useAsCStringLen bytes $ \(source, n) -> copyBytes p source n >> pokeByteOff p n (0 :: Word8)

-- | The longest prefix of the text's UTF-8 of at most the given length that
-- ends between characters.
fitUtf8 :: Int -> Text -> B.ByteString
fitUtf8 limit t
  | B.length bytes <= limit = bytes
  | otherwise = B.take (head [i | i <- [limit, limit - 1 .. 0], startsCharacter i]) bytes
  where
    bytes = encodeUtf8 t
    startsCharacter i = i == 0 || B.index bytes i .&. 0xC0 /= 0x80

-- | Navigation controls have semantic names, independent of their artwork.
data Navigation = NewChatButton | SettingsButton
  deriving (Eq, Show)

-- | A compact button with a generous hit target and a drawn icon.
navigationButton :: Navigation -> IO Bool
navigationButton kind = do
  h <- getTextLineHeight
  pressed <- withStyleVec ImGuiStyleVar_FramePadding (ImVec2 17 ((diameter - h) / 2)) $
    withStyleFloat ImGuiStyleVar_FrameBorderSize 0 $
      bracket_ (with (ImVec4 0 0 0 0) (Raw.pushStyleColor ImGuiCol_Button)) (Raw.popStyleColor 1) $
        button name
  ImVec2 x0 y0 <- Raw.getItemRectMin
  list <- Raw.getWindowDrawList
  ink <- styleColourU32 ImGuiCol_Text
  let cx = x0 + diameter / 2
      cy = y0 + diameter / 2
      scale = 0.70
      at px py = ImVec2 (cx + px * scale) (cy + py * scale)
      stroke closed points = do
        DrawList.pathClear list
        mapM_ (`with` DrawList.pathLineTo list) points
        DrawList.pathStroke list ink closed 1.3
  case kind of
    SettingsButton -> do
      let tooth i =
            let angle = fromIntegral i * 2 * pi / 48
                radius = if i `mod` 6 `elem` [1, 2, 3] then 9 else 6.8
            in at (cos angle * radius) (sin angle * radius)
      stroke ImDrawFlags_Closed (map tooth [0 :: Int .. 47])
      with (at 0 0) $ \centre -> DrawList.addCircle list centre (realToFrac (3 * scale)) ink 0 1.3
    NewChatButton -> do
      stroke ImDrawFlags_None [at 1 (-7), at (-7) (-7), at (-7) 7, at 7 7, at 7 1]
      stroke ImDrawFlags_Closed [at (-2) 1, at 6 (-7), at 9 (-4), at 1 4, at (-3) 5]
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
    bracket_ (with (ImVec4 0 0 0 0) (Raw.pushStyleColor ImGuiCol_FrameBg)) (Raw.popStyleColor 1) $
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
