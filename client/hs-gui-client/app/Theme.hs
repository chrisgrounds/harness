{-# LANGUAGE PatternSynonyms #-}

-- | The packaged palette, the shades derived from it, and Dear ImGui's
-- style made from them.
module Theme (loadPalette, applyTheme, metrics, raised, lifted, seam, mix, vec4) where

import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8')
import DearImGui hiding (end, flags, indent, size, w, x, y, z)
import ImGui.Extras (Metrics (..), setColour, setMetrics)
import Paths_hs_gui_client (getDataFileName)
import Theme.Palette

-- | Loads the default palette from the packaged TOML file.
loadPalette :: IO Palette
loadPalette = do
  path <- getDataFileName "themes/orng.toml"
  contents <- B.readFile path
  let invalid err = fail ("invalid theme " ++ path ++ ": " ++ err)
  either (invalid . show) (either invalid pure . parsePalette) (decodeUtf8' contents)

-- | The interface's sizes, in points.
metrics :: Metrics
metrics =
  Metrics
    { windowPadding = 14,
      framePadding = (12, 7),
      itemSpacing = (8, 7),
      rounding = 10,
      scrollbarSize = 8
    }

-- | The sidebar and the composer: a step up from the window.
raised :: Palette -> Colour
raised p = mix 0.045 (foreground p) (background p)

-- | A hovered row in the sidebar.
lifted :: Palette -> Colour
lifted p = mix 0.09 (foreground p) (background p)

-- | Rails, hairlines and the composer's edge.
seam :: Palette -> Colour
seam p = mix 0.14 (foreground p) (background p)

-- | Sets Dear ImGui's colours from the palette, and its sizes from 'metrics'.
applyTheme :: Palette -> IO ()
applyTheme p = do
  setMetrics metrics
  mapM_ (\(col, c, alpha) -> let (r, g, b) = unit c in setColour col r g b alpha) colours
  where
    colours =
      [ (ImGuiCol_Text, foreground p, 1),
        (ImGuiCol_TextDisabled, muted p, 1),
        (ImGuiCol_WindowBg, background p, 1),
        (ImGuiCol_ChildBg, background p, 0),
        (ImGuiCol_PopupBg, raised p, 1),
        (ImGuiCol_Border, seam p, 1),
        (ImGuiCol_BorderShadow, background p, 0),
        (ImGuiCol_FrameBg, raised p, 1),
        (ImGuiCol_FrameBgHovered, raised p, 1),
        (ImGuiCol_FrameBgActive, raised p, 1),
        (ImGuiCol_TitleBg, raised p, 1),
        (ImGuiCol_TitleBgActive, raised p, 1),
        (ImGuiCol_TitleBgCollapsed, raised p, 1),
        (ImGuiCol_ScrollbarBg, background p, 0),
        (ImGuiCol_ScrollbarGrab, seam p, 1),
        (ImGuiCol_ScrollbarGrabHovered, muted p, 1),
        (ImGuiCol_ScrollbarGrabActive, muted p, 1),
        (ImGuiCol_CheckMark, accent p, 1),
        (ImGuiCol_Button, raised p, 1),
        (ImGuiCol_ButtonHovered, lifted p, 1),
        (ImGuiCol_ButtonActive, seam p, 1),
        (ImGuiCol_Header, lifted p, 1),
        (ImGuiCol_HeaderHovered, lifted p, 1),
        (ImGuiCol_HeaderActive, seam p, 1),
        (ImGuiCol_Separator, seam p, 1),
        (ImGuiCol_SeparatorHovered, seam p, 1),
        (ImGuiCol_SeparatorActive, seam p, 1),
        (ImGuiCol_ResizeGrip, seam p, 0),
        (ImGuiCol_ResizeGripHovered, seam p, 0),
        (ImGuiCol_ResizeGripActive, seam p, 0),
        (ImGuiCol_PlotHistogram, accent p, 1),
        (ImGuiCol_TextSelectedBg, highlight p, 0.3),
        (ImGuiCol_NavCursor, accent p, 0),
        (ImGuiCol_ModalWindowDimBg, background p, 0.6)
      ]

-- | @mix t a b@ is @t@ of the way from @b@ to @a@.
mix :: Double -> Colour -> Colour -> Colour
mix t a b = Colour (channel red) (channel green) (channel blue)
  where
    channel f = round (t * fromIntegral (f a) + (1 - t) * fromIntegral (f b))

unit :: Colour -> (Float, Float, Float)
unit c = (f (red c), f (green c), f (blue c))
  where
    f v = fromIntegral v / 255

vec4 :: Colour -> ImVec4
vec4 c = let (r, g, b) = unit c in ImVec4 r g b 1
