{-# LANGUAGE OverloadedStrings #-}

-- | Turns an 'Artefact' into Haskell source.
module Render (renderFile, renderNewModule, renderChanged, renderProperties, designSummary) where

import Control.Monad (foldM)
import Core
import Data.List (dropWhileEnd, nub)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Decls

-- | A file the run writes: a new module, or an existing one (given its
-- original source) with the run's changes.
renderFile :: Artefact -> FilePath -> Maybe Text -> Either Text Text
renderFile art path original = case (plan, original) of
  (Just p, _) | planIsNew p -> Right (renderNewModule p fns)
  (_, Just source) -> renderChanged source plan fns
  (_, Nothing) -> Left ("No source for " <> T.pack path)
  where
    plan = case [p | p <- modules art, planPath p == path] of
      p : _ -> Just p
      [] -> Nothing
    fns = [f | f <- functions art, refPath (fnRef f) == path]

-- | A new module, with @undefined@ for functions not yet implemented.
renderNewModule :: ModulePlan -> [FunctionSpec] -> Text
renderNewModule plan fns = T.unlines (header ++ concatMap (\f -> functionLines f ++ [""]) fns)
  where
    importLines = nub (map T.strip (planImports plan ++ concatMap fnImports fns))
    header =
      map pragma (planExtensions plan)
        ++ ["module " <> moduleNameText (planModule plan) <> " where", ""]
        ++ (if null importLines then [] else importLines ++ [""])
        ++ T.lines (T.stripEnd (planTypes plan))
        ++ [""]

-- | An existing module with the run's changes. Type declarations and
-- designed functions replace the declaration of the same name or are added
-- after the others, and are exported if new; repairs keep their comments
-- and signature and get new equations. Every other line is left as it was.
renderChanged :: Text -> Maybe ModulePlan -> [FunctionSpec] -> Either Text Text
renderChanged original plan fns = do
  repaired <- foldM repair (foldl (flip upsertDecl) source (typeDecls ++ map designedDecl designed)) repairs
  pure . joinSource $
    addExports newExports . addImports (maybe [] planImports plan ++ concatMap fnImports fns) . addPragmas (maybe [] planExtensions plan) $
      repaired
  where
    source = splitSource original
    designed = [f | f <- fns, fnKind f == Designed]
    repairs = [f | f <- fns, fnKind f == Repair]
    typeDecls = case splitSource (maybe "" planTypes plan) of
      SourceFile leading (d : ds) -> map tidy (d {declLines = filter (not . T.null . T.strip) leading ++ declLines d} : ds)
      SourceFile _ [] -> []
    tidy d = d {declLines = dropWhileEnd (T.null . T.strip) (declLines d)}
    designedDecl f = Decl ValueDecl (Just (functionNameText (fnName f))) (functionLines f) 0
    repair sf f = case break ((== Just name) . declKey) (sourceDecls sf) of
      (before, d : after) -> Right sf {sourceDecls = before ++ rewriteEquations name (fnBody f) d : after}
      (_, []) -> Left ("No declaration of " <> name <> " to repair.")
      where
        name = functionNameText (fnName f)
    newExports =
      [e | d <- typeDecls, Just key <- [declKey d], isNothing (findDecl key source), Just e <- [exportName d]]
        ++ [name | f <- designed, let name = functionNameText (fnName f), isNothing (findDecl name source)]

functionLines :: FunctionSpec -> [Text]
functionLines f = functionHead f ++ maybe [name <> " = undefined"] (T.lines . T.stripEnd) (fnBody f)
  where
    name = functionNameText (fnName f)

-- | A function's documentation and signature.
functionHead :: FunctionSpec -> [Text]
functionHead f = docLines ++ T.lines (functionNameText (fnName f) <> " :: " <> T.strip (fnSignature f))
  where
    docLines = case T.lines (T.strip (fnDoc f)) of
      [] -> []
      first : rest -> ("-- | " <> first) : map ("--   " <>) rest

-- | Renders the property suite: a @Main@ module that imports every module in
-- the plan (except executables' @Main@ modules), checks every property and
-- prints @=== name@ before each, so failures can be traced to properties.
renderProperties :: Artefact -> Text
renderProperties art =
  T.unlines $
    map pragma (nub (concatMap planExtensions (modules art)))
      ++ [ "{-# OPTIONS_GHC -Wno-orphans #-}",
           "module Main (main) where",
           ""
         ]
      ++ nub ["import " <> name | ModuleName name <- map planModule (modules art), name /= "Main"]
      ++ [ "import Test.QuickCheck",
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

-- | Every module's new and changed types, and the signature and
-- documentation of every designed function, for implementers working on
-- one function.
designSummary :: Artefact -> Text
designSummary art = T.intercalate "\n" (map summarise (modules art))
  where
    summarise plan =
      T.unlines $
        [ "-- " <> moduleNameText (planModule plan) <> " (" <> (if planIsNew plan then "new" else "changed") <> ", " <> T.pack (planPath plan) <> ")",
          T.stripEnd (planTypes plan)
        ]
          ++ concat
            [ functionHead f
            | f <- functions art,
              fnKind f == Designed,
              refPath (fnRef f) == planPath plan
            ]

pragma :: Text -> Text
pragma ext = "{-# LANGUAGE " <> T.strip ext <> " #-}"
