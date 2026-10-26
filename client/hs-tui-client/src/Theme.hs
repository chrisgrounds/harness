{-# LANGUAGE OverloadedStrings #-}

module Theme (loadTheme, accent, muted, metadataAttr, borderAttr, userAttr, toolAttr, bodyAttr, danger, successAttr) where

import Brick
import Brick.Widgets.Edit
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Paths_hs_tui_client (getDataFileName)
import Toml (Table, Value, parse, Table'(..), Value'(..), forgetTableAnns)

accent, muted, metadataAttr, borderAttr, userAttr, toolAttr, bodyAttr, danger, successAttr :: AttrName
accent = attrName "accent"
muted = attrName "muted"
metadataAttr = attrName "metadata"
borderAttr = attrName "border"
userAttr = attrName "user"
toolAttr = attrName "tool"
bodyAttr = attrName "body"
danger = attrName "danger"
successAttr = attrName "success"

-- | Load the default theme from the packaged TOML file and turn it into an
-- 'AttrMap'.
loadTheme :: IO AttrMap
loadTheme = do
  path <- getDataFileName "themes/orng.toml"
  contents <- readFile path
  case parse (T.pack contents) of
    Left err -> fail ("invalid theme " ++ path ++ ": " ++ err)
    Right tab -> pure (themeFromTable (forgetTableAnns tab))

themeFromTable :: Table -> AttrMap
themeFromTable tab = attrMap defaultAttr entries
  where
    (bR, bG, bB) = paletteRGB "background" tab
    (tR, tG, tB) = paletteRGB "text" tab
    defaultAttr = V.withBackColor (fg (V.linearColor tR tG tB)) (V.linearColor bR bG bB)
    styled paletteName bold' = styleAttr (Style (paletteRGB paletteName tab) bold')
    entries =
      [ (accent, styled "orange" True),
        (muted, styled "muted" False),
        (metadataAttr, styled "border" False),
        (borderAttr, styled "border" False),
        (userAttr, styled "cyan" True),
        (toolAttr, styled "secondaryOrange" False),
        (bodyAttr, styled "muted" False),
        (danger, styled "error" False),
        (successAttr, styled "success" False),
        (scrollbarAttr, styled "border" False),
        (scrollbarHandleAttr, styled "cyan" False),
        (editAttr, styled "text" False),
        (editFocusedAttr, styled "text" False)
      ]

data Style = Style (Int, Int, Int) Bool

styleAttr :: Style -> V.Attr
-- Vty's rgbColor quantizes to its 240-color palette (including lifting our
-- dark background to grey). linearColor preserves the theme's exact bytes;
-- Vty handles conversion at output time for terminals without true color.
styleAttr (Style (r, g, b) True) = V.withStyle (fg (V.linearColor r g b)) V.bold
styleAttr (Style (r, g, b) False) = fg (V.linearColor r g b)

paletteRGB :: Text -> Table -> (Int, Int, Int)
paletteRGB name palette = rgbOf (require name palette)

rgbOf :: Value -> (Int, Int, Int)
rgbOf (List' _ [Integer' _ r, Integer' _ g, Integer' _ b]) = (fromInteger r, fromInteger g, fromInteger b)
rgbOf _ = error "theme: color must be an [r, g, b] array"

require :: Text -> Table -> Value
require key t = maybe (error ("theme: missing key " ++ T.unpack key)) id (tableLookup key t)

tableLookup :: Text -> Table -> Maybe Value
tableLookup key (MkTable t) = snd <$> Map.lookup key t
