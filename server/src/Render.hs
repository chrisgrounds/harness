{-# LANGUAGE OverloadedStrings #-}

-- | Turns an 'Artefact' into Haskell source.
module Render (Span, renderModule, renderProperties) where

import Core
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

-- | The first and last line (1-based, inclusive) of a function's signature
-- and definition in the rendered module.
type Span = (Int, Int)

-- | Renders the module, with @undefined@ for functions not yet implemented,
-- and where each function ends up.
renderModule :: ModuleName -> Artefact -> (Text, [(FunctionName, Span)])
renderModule (ModuleName name) art = (T.unlines (header ++ concat blocks), spans)
  where
    importLines = nub (map T.strip (imports art ++ concatMap fnImports (functions art)))
    header =
      map pragma (extensions art)
        ++ ["module " <> name <> " where", ""]
        ++ (if null importLines then [] else importLines ++ [""])
        ++ [T.stripEnd (typesSource art), ""]
    rendered = map renderFunction (functions art)
    blocks = map (\(_, docLines, defLines) -> docLines ++ defLines ++ [""]) rendered
    starts = scanl (+) (length header + 1) (map length blocks)
    spans =
      [ (fnName f, (start + length docLines, start + length docLines + length defLines - 1))
      | (f, (_, docLines, defLines), start) <- zip3 (functions art) rendered starts
      ]

renderFunction :: FunctionSpec -> (FunctionName, [Text], [Text])
renderFunction f = (fnName f, docLines, signature ++ definition)
  where
    name = functionNameText (fnName f)
    docLines = case T.lines (T.strip (fnDoc f)) of
      [] -> []
      first : rest -> ("-- | " <> first) : map ("--   " <>) rest
    signature = T.lines (name <> " :: " <> T.strip (fnSignature f))
    definition = maybe [name <> " = undefined"] (T.lines . T.stripEnd) (fnBody f)

-- | Renders the property suite: a @Main@ module that checks every property and
-- prints @=== name@ before each, so failures can be traced to properties.
renderProperties :: ModuleName -> Artefact -> Text
renderProperties (ModuleName name) art =
  T.unlines $
    map pragma (extensions art)
      ++ [ "{-# OPTIONS_GHC -Wno-orphans #-}",
           "module Main (main) where",
           "",
           "import " <> name,
           "import Test.QuickCheck",
           "import qualified Test.QuickCheck as HoleyQuickCheck",
           "import qualified System.Exit as HoleyExit"
         ]
      ++ nub (map T.strip (propertyImports art))
      ++ ["", T.stripEnd (propertyHelpers art), ""]
      ++ concatMap (\p -> T.lines (T.stripEnd (propSource p)) ++ [""]) (properties art)
      ++ [ "main :: IO ()",
           "main = do",
           "  results <-",
           "    sequence",
           "      [ " <> T.intercalate "\n      , " (map check (properties art)),
           "      ]",
           "  if all HoleyQuickCheck.isSuccess results then HoleyExit.exitSuccess else HoleyExit.exitFailure"
         ]
  where
    check p = "putStrLn " <> T.pack (show ("=== " <> propName p)) <> " >> HoleyQuickCheck.quickCheckResult " <> propName p

pragma :: Text -> Text
pragma ext = "{-# LANGUAGE " <> T.strip ext <> " #-}"
