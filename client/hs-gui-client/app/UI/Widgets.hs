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
    composer,
  )
where

import Control.Exception (bracket_)
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
import ImGui.Extras (Metrics (..), colourU32, composerBox, handCursor)
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
        withText hint $ \hintText ->
          maybe ($ nullPtr) B.useAsCString fitted $ \replacing ->
            composerBox name hintText p capacity width height focus replacing
    contents <- decodeUtf8With lenientDecode <$> B.packCString p
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
