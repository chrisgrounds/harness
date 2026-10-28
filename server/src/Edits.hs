{-# LANGUAGE OverloadedStrings #-}

-- | The run-changing tools: decoding their arguments and applying them to a
-- 'Run'. Rejections are returned to the model so it can correct itself.
module Edits
  ( TypesArgs (..),
    FunctionArg (..),
    FunctionsArgs (..),
    PropertiesArgs (..),
    ImplementArgs (..),
    decodeArgs,
    applyTypes,
    applyFunctions,
    applyProperties,
    applyImplementation,
  )
where

import Control.Monad (unless, when)
import Core
import Data.Aeson
import Data.Char (isAlphaNum, isLower)
import Data.List (find, nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Project (Component, componentName, validModuleName)

data TypesArgs = TypesArgs
  { typesComponent :: Text,
    typesModule :: Text,
    typesExtensions :: [Text],
    typesImports :: [Text],
    typesBody :: Text
  }

instance FromJSON TypesArgs where
  parseJSON = withObject "SetTypes" $ \o ->
    TypesArgs <$> o .: "component" <*> o .: "module" <*> o .:? "extensions" .!= [] <*> o .:? "imports" .!= [] <*> o .: "types"

data FunctionArg = FunctionArg Text Text Text

instance FromJSON FunctionArg where
  parseJSON = withObject "function" $ \o ->
    FunctionArg <$> o .: "name" <*> o .: "signature" <*> o .:? "doc" .!= ""

newtype FunctionsArgs = FunctionsArgs [FunctionArg]

instance FromJSON FunctionsArgs where
  parseJSON = withObject "SetFunctions" $ \o -> FunctionsArgs <$> o .: "functions"

data PropertiesArgs = PropertiesArgs [Text] Text [PropertySpec]

instance FromJSON PropertiesArgs where
  parseJSON = withObject "SetProperties" $ \o ->
    PropertiesArgs <$> o .:? "imports" .!= [] <*> o .:? "helpers" .!= "" <*> (o .: "properties" >>= mapM property)
    where
      property = withObject "property" $ \p -> PropertySpec <$> p .: "name" <*> p .: "source"

data ImplementArgs = ImplementArgs Text [Text]

instance FromJSON ImplementArgs where
  parseJSON = withObject "ImplementFunction" $ \o ->
    ImplementArgs <$> o .: "definition" <*> o .:? "imports" .!= []

decodeArgs :: (FromJSON a) => Text -> Either Text a
decodeArgs args = either (Left . ("Invalid arguments: " <>) . T.pack) Right (eitherDecodeStrict (TE.encodeUtf8 args))

-- | Sets the module's location, imports and types. Once a run has written its
-- module, it cannot move it.
applyTypes :: [Component] -> TypesArgs -> Run -> Either Text Run
applyTypes components args run = do
  component <-
    maybe
      (Left ("Unknown component " <> typesComponent args <> ". Choose one of: " <> T.intercalate ", " (map componentName components)))
      Right
      (find ((== T.strip (typesComponent args)) . componentName) components)
  let name = T.strip (typesModule args)
  unless (validModuleName name) $ Left ("Invalid module name: " <> name)
  case (targetModule run, targetComponent run) of
    (Just (ModuleName existing), Just existingComponent)
      | not (null (touchedFiles run)) && (existing, existingComponent) /= (name, componentName component) ->
          Left ("This run has already written " <> existing <> " in " <> existingComponent <> "; keep that component and module.")
    _ -> pure ()
  importLines <- traverse checkImport (typesImports args)
  when (any (("import " `T.isPrefixOf`) . T.strip) (T.lines (typesBody args))) $
    Left "Put imports in the imports field, not in types."
  when (any (("module " `T.isPrefixOf`) . T.strip) (T.lines (typesBody args))) $
    Left "Leave out the module header; it is generated."
  let art = artefact run
  pure
    run
      { targetComponent = Just (componentName component),
        targetModule = Just (ModuleName name),
        artefact = art {extensions = map T.strip (typesExtensions args), imports = importLines, typesSource = typesBody args}
      }

-- | Replaces the function list. Every body is cleared, since the design changed.
applyFunctions :: FunctionsArgs -> Run -> Either Text Run
applyFunctions (FunctionsArgs args) run = do
  when (null args) $ Left "Declare at least one function."
  specs <- traverse spec args
  let names = map fnName specs
  when (length (nub names) /= length names) $ Left "Function names must be unique."
  pure run {artefact = (artefact run) {functions = specs}}
  where
    spec (FunctionArg name signature doc) = do
      let n = T.strip name
      unless (validFunctionName n) $ Left ("Invalid function name: " <> n)
      let sig = stripPrefixes [n <> " ::", n <> "::", "::"] (T.strip signature)
      when (T.null sig) $ Left ("Missing signature for " <> n)
      pure (FunctionSpec (FunctionName n) sig (T.strip doc) Nothing [])

applyProperties :: PropertiesArgs -> Run -> Either Text Run
applyProperties (PropertiesArgs importLines helpers props) run = do
  checkedImports <- traverse checkImport importLines
  checked <- traverse check props
  let names = map propName checked
  when (length (nub names) /= length names) $ Left "Property names must be unique."
  pure run {artefact = (artefact run) {propertyImports = checkedImports, propertyHelpers = helpers, properties = checked}}
  where
    check (PropertySpec name source) = do
      let n = T.strip name
      unless ("prop_" `T.isPrefixOf` n && validFunctionName n) $ Left ("Property names must start with prop_: " <> n)
      unless (any ((n `T.isPrefixOf`) . T.strip) (T.lines source)) $ Left ("The source of " <> n <> " must define " <> n <> ".")
      pure (PropertySpec n source)

-- | Sets one function's definition, dropping any type signature the model repeated.
applyImplementation :: FunctionName -> ImplementArgs -> Run -> Either Text Run
applyImplementation name (ImplementArgs definition importLines) run = do
  checkedImports <- traverse checkImport importLines
  let n = functionNameText name
      body = T.unlines (filter (not . isSignature n) (T.lines definition))
  unless (any ((n `T.isPrefixOf`) . T.stripStart) (T.lines body)) $
    Left ("The definition must define " <> n <> ".")
  unless (any ((== name) . fnName) (functions (artefact run))) $
    Left ("Unknown function " <> n)
  let set f
        | fnName f == name = f {fnBody = Just (T.stripEnd body), fnImports = checkedImports}
        | otherwise = f
  pure run {artefact = (artefact run) {functions = map set (functions (artefact run))}}
  where
    isSignature n line = any (`T.isPrefixOf` T.strip line) [n <> " ::", n <> "::"]

checkImport :: Text -> Either Text Text
checkImport line
  | "import " `T.isPrefixOf` T.strip line = Right (T.strip line)
  | otherwise = Left ("Not an import line: " <> line)

validFunctionName :: Text -> Bool
validFunctionName name = case T.uncons name of
  Just (c, rest) -> (isLower c || c == '_') && T.all (\x -> isAlphaNum x || x == '_' || x == '\'') rest
  Nothing -> False

stripPrefixes :: [Text] -> Text -> Text
stripPrefixes prefixes value = case [rest | p <- prefixes, Just rest <- [T.stripPrefix p value]] of
  rest : _ -> T.strip rest
  [] -> value
