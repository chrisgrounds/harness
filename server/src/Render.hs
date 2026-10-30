{-# LANGUAGE OverloadedStrings #-}

module Render (renderFile, renderNewModule, renderChanged, renderProperties, designSummary, usedTypes) where

import Control.Monad (foldM)
import Core
import Data.List (dropWhileEnd, isPrefixOf, nub)
import Data.Char (isUpper)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Decls

renderFile :: Artefact -> FilePath -> Maybe Text -> Either Text Text
renderFile art path original = case (plan, original) of
  (Just p, _) | planIsNew p -> Right (renderNewModule p fns)
  (_, Just source) -> renderChanged (usedOutside art path) source plan fns
  (_, Nothing) -> Left ("No source for " <> T.pack path)
  where
    plan = case [p | p <- modules art, planPath p == path] of
      p : _ -> Just p
      [] -> Nothing
    fns = [f | f <- functions art, refPath (fnRef f) == path]

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

usedOutside :: Artefact -> FilePath -> Text -> Bool
usedOutside art path name = name `elem` concatMap freeNames texts
  where
    texts =
      propertyHelpers art
        : map propSource (properties art)
        ++ concat [fnSignature f : maybe [] pure (fnBody f) | f <- functions art, refPath (fnRef f) /= path]
        ++ [planTypes p | p <- modules art, planPath p /= path]

renderChanged :: (Text -> Bool) -> Text -> Maybe ModulePlan -> [FunctionSpec] -> Either Text Text
renderChanged used original plan fns = do
  repaired <- foldM repair (foldl (flip upsertDecl) source (typeDecls ++ map designedDecl designed)) repairs
  pure . joinSource $
    addExports newExports . addImports (maybe [] planImports plan ++ concatMap fnImports fns) . addPragmas (maybe [] planExtensions plan) $
      repaired
  where
    source = splitSource original
    designed = [f | f <- fns, fnKind f == Designed]
    repairs = [f | f <- fns, fnKind f == Repair]
    typeDecls = map keepComments $ case splitSource (maybe "" planTypes plan) of
      SourceFile leading (d : ds) -> map tidy (d {declLines = filter (not . T.null . T.strip) leading ++ declLines d} : ds)
      SourceFile _ [] -> []
    keepComments d = case declKey d >>= (`findDecl` source) of
      Just old | not (leadingComments old `isPrefixOf` declLines d) -> d {declLines = leadingComments old ++ declLines d}
      _ -> d
    tidy d = d {declLines = dropWhileEnd (T.null . T.strip) (declLines d)}
    designedDecl f = Decl ValueDecl (Just name) (docs ++ signatureLines f ++ body) 0
      where
        name = functionNameText (fnName f)
        docs = maybe [] leadingComments (findDecl name source)
        kept = do
          old <- findDecl name source
          adaptEquations name (fnSignature f) old
        body = case (fnBody f, fnPlaceholder f, kept) of
          (Just b, _, _) -> T.lines (T.stripEnd b)
          (Nothing, PreviousBody, Just equations) -> equations
          _ -> [name <> " = undefined"]
    repair sf f = case break ((== Just name) . declKey) (sourceDecls sf) of
      (before, d : after)
        | declKind d == InstanceDecl -> case rewriteInstance (fnBody f) d of
            Just d' -> Right sf {sourceDecls = before ++ d' : after}
            Nothing -> Left ("Cannot repair " <> name <> ": it has no methods.")
        | otherwise -> Right sf {sourceDecls = before ++ rewriteEquations name (fnBody f) d : after}
      (_, []) -> Left ("No declaration of " <> name <> " to repair.")
      where
        name = functionNameText (fnName f)
    newExports =
      [e | d <- typeDecls, Just key <- [declKey d], isNothing (findDecl key source) || any used (key : constructors d), Just e <- [exportName d]]
        ++ [name | f <- designed, let name = functionNameText (fnName f), isNothing (findDecl name source) || used name]
    constructors d = [w | w <- concatMap (T.split (not . isIdentChar)) (declLines d), maybe False (isUpper . fst) (T.uncons w)]

functionLines :: FunctionSpec -> [Text]
functionLines f = signatureLines f ++ maybe [name <> " = undefined"] (T.lines . T.stripEnd) (fnBody f)
  where
    name = functionNameText (fnName f)

functionHead :: FunctionSpec -> [Text]
functionHead f = docLines f ++ signatureLines f

docLines :: FunctionSpec -> [Text]
docLines f = case T.lines (T.strip (fnDoc f)) of
  [] -> []
  first : rest -> ("-- | " <> first) : map ("--   " <>) rest

signatureLines :: FunctionSpec -> [Text]
signatureLines f = T.lines (functionNameText (fnName f) <> " :: " <> T.strip (fnSignature f))

renderProperties :: [ModuleName] -> Artefact -> Text
renderProperties visible art =
  T.unlines $
    map pragma (nub (concatMap planExtensions (modules art)))
      ++ [ "{-# OPTIONS_GHC -Wno-orphans #-}",
           "module Main (main) where",
           ""
         ]
      ++ nub ["import " <> name | ModuleName name <- map planModule (modules art), ModuleName name `elem` visible, name /= "Main"]
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

usedTypes :: [Text] -> Artefact -> FunctionSpec -> Text
usedTypes sources art fn = T.intercalate "\n" [T.unlines (declLines d) | d <- take 12 (go (typeNames (fnSignature fn)) [])]
  where
    known = [(key, d) | s <- sources, d <- sourceDecls (splitSource s), declKind d `elem` [DataDecl, TypeDecl], Just key <- [declKey d]]
    designed = [key | p <- modules art, d <- sourceDecls (splitSource (planTypes p)), Just key <- [declKey d]]
    go [] _ = []
    go (name : rest) seen
      | name `elem` seen || name `elem` designed = go rest seen
      | otherwise = case lookup name known of
          Just d -> d : go (rest ++ typeNames (T.unlines (declLines d))) (name : seen)
          Nothing -> go rest (name : seen)

pragma :: Text -> Text
pragma ext = "{-# LANGUAGE " <> T.strip ext <> " #-}"
