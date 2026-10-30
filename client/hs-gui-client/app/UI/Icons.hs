{-# LANGUAGE PatternSynonyms #-}

-- | Status icons, drawn as shapes rather than characters so they do not
-- depend on the font.
module UI.Icons (Icon (..), icon, iconWidth) where

import DearImGui hiding (end, flags, indent, size, w, x, y, z)
import qualified DearImGui.Raw as Raw
import qualified DearImGui.Raw.DrawList as DrawList
import Foreign (with)
import ImGui.Extras (Metrics (..))
import Theme (metrics)
import Theme.Palette (Colour)
import UI.Draw (colour, stroke)

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
      path = stroke list ink thickness ImDrawFlags_None
      arc from to = do
        DrawList.pathClear list
        with (at 0.5 0.5) $ \centre -> DrawList.pathArcTo list centre radius (realToFrac from) (realToFrac to) 0
        DrawList.pathStroke list ink thickness ImDrawFlags_None
  case kind of
    Hollow -> with (at 0.5 0.5) $ \centre -> DrawList.addCircle list centre radius ink 0 thickness
    Dot -> with (at 0.5 0.5) $ \centre -> DrawList.addCircleFilled list centre (radius * 0.8) ink 0
    Spinner -> let start = realToFrac time * 6 :: Float in arc start (start + 4.4)
    Tick -> path [at 0.24 0.53, at 0.42 0.71, at 0.77 0.31]
    Cross -> path [at 0.29 0.29, at 0.71 0.71] >> path [at 0.71 0.29, at 0.29 0.71]
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
      path [at 0.5 0.16, at 0.88 0.84, at 0.12 0.84, at 0.5 0.16]
      path [at 0.5 0.4, at 0.5 0.6]
      with (at 0.5 0.72) $ \dot -> DrawList.addCircleFilled list dot (realToFrac (h * 0.05)) ink 0
  Raw.sameLine
  pure (at 0.5 0.5, r)
