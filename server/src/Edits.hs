{-# LANGUAGE OverloadedStrings #-}

-- | The run-changing tools: decoding their arguments and applying them to a
-- 'Run'. Rejections are returned to the model so it can correct itself.
module Edits
  ( TypesArgs (..),
    ModuleArg (..),
    FunctionArg (..),
    FunctionsArgs (..),
    PropertiesArgs (..),
    ImplementArgs (..),
    RefactorArgs (..),
    GapArgs (..),
    RefactorArg (..),
    decodeArgs,
    checkModules,
    applyModules,
    applyFunctions,
    applyProperties,
    applyImplementation,
    applyRefactor,
    addRepairs,
  )
where

import Control.Monad (unless, when)
import Core
import Data.Aeson
import Data.Char (isLower)
import Decls (Decl (..), SourceFile (..), isIdentChar, splitSource)
import Data.List (find, nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Project (Component, componentName, validModuleName)

newtype TypesArgs = TypesArgs [ModuleArg]

instance FromJSON TypesArgs where
  parseJSON = withObject "SetTypes" $ \o -> TypesArgs <$> o .: "modules"

data ModuleArg = ModuleArg
  { argComponent :: Text,
    argModule :: Text,
    argExtensions :: [Text],
    argImports :: [Text],
    argTypes :: Text
  }

instance FromJSON ModuleArg where
  parseJSON = withObject "module" $ \o ->
    ModuleArg <$> o .: "component" <*> o .: "module" <*> o .:? "extensions" .!= [] <*> o .:? "imports" .!= [] <*> o .:? "types" .!= ""

data FunctionArg = FunctionArg
  { argFunctionModule :: Text,
    argFunctionName :: Text,
    argSignature :: Text,
    argDoc :: Text,
    -- | For an existing function, what to change.
    argChange :: Text
  }

instance FromJSON FunctionArg where
  parseJSON = withObject "function" $ \o ->
    FunctionArg <$> o .: "module" <*> o .: "name" <*> o .: "signature" <*> o .:? "doc" .!= "" <*> o .:? "change" .!= ""

newtype FunctionsArgs = FunctionsArgs [FunctionArg]

instance FromJSON FunctionsArgs where
  parseJSON = withObject "SetFunctions" $ \o -> FunctionsArgs <$> o .: "functions"

data PropertiesArgs = PropertiesArgs
  { argPropertyImports :: [Text],
    argHelpers :: Text,
    argProperties :: [PropertySpec]
  }

instance FromJSON PropertiesArgs where
  parseJSON = withObject "SetProperties" $ \o ->
    PropertiesArgs <$> o .:? "imports" .!= [] <*> o .:? "helpers" .!= "" <*> (o .: "properties" >>= mapM property)
    where
      property = withObject "property" $ \p -> PropertySpec <$> p .: "name" <*> p .: "source"

data ImplementArgs = ImplementArgs
  { argDefinition :: Text,
    argImplementationImports :: [Text]
  }

instance FromJSON ImplementArgs where
  parseJSON = withObject "ImplementFunction" $ \o ->
    ImplementArgs <$> o .: "definition" <*> o .:? "imports" .!= []

decodeArgs :: (FromJSON a) => Text -> Either Text a
decodeArgs args = either (Left . ("Invalid arguments: " <>) . T.pack) Right (eitherDecodeStrict (TE.encodeUtf8 args))

-- | The component and name of each module, checked, for the caller to
-- find on disk.
checkModules :: [Component] -> TypesArgs -> Either Text [(Component, ModuleName, ModuleArg)]
checkModules components (TypesArgs args) = do
  when (null args) $ Left "List at least one module."
  checked <- traverse check args
  let names = [name | (_, name, _) <- checked]
  unique "List each module once." names
  pure checked
  where
    check arg = do
      component <-
        maybe
          (Left ("Unknown component " <> argComponent arg <> ". Choose one of: " <> T.intercalate ", " (map (componentNameText . componentName) components)))
          Right
          (find ((== ComponentName (T.strip (argComponent arg))) . componentName) components)
      let name = T.strip (argModule arg)
      unless (validModuleName name) $ Left ("Invalid module name: " <> name)
      when (any (("import " `T.isPrefixOf`) . T.strip) (T.lines (argTypes arg))) $
        Left ("Put the imports of " <> name <> " in its imports field, not in types.")
      when (any (("module " `T.isPrefixOf`) . T.strip) (T.lines (argTypes arg))) $
        Left ("Leave out the module header of " <> name <> "; it is generated or kept.")
      _ <- traverse checkImport (argImports arg)
      pure (component, ModuleName name, arg)

-- | Sets the modules the run creates or changes and their imports and
-- types, dropping the repairs found for the previous design and functions
-- in modules no longer listed. A run cannot drop a module it has already
-- created.
applyModules :: [ModulePlan] -> Run -> Either Text Run
applyModules plans run = do
  let paths = map planPath plans
  unique "Two modules have the same file." paths
  case [planPath p | p <- modules (artefact run), planIsNew p, planPath p `elem` touchedFiles run, planPath p `notElem` paths] of
    [] -> pure ()
    dropped -> Left ("This run has already created " <> T.intercalate ", " (map T.pack dropped) <> "; keep those modules.")
  let art = artefact run
      tidy p = p {planExtensions = map T.strip (planExtensions p), planImports = map T.strip (planImports p)}
  pure run {artefact = art {modules = map tidy plans, functions = [f | f <- functions art, fnKind f == Designed, refPath (fnRef f) `elem` paths]}}

-- | Replaces the function list. Every body is cleared, since the design
-- changed. A function named like an existing one in its module replaces it;
-- the previous declaration is kept for its implementer.
applyFunctions :: (FunctionRef -> Maybe Text) -> FunctionsArgs -> Run -> Either Text Run
applyFunctions previous (FunctionsArgs args) run = do
  when (null args) $ Left "Declare at least one function."
  specs <- traverse spec args
  let refs = map fnRef specs
  unique "Each function may be declared once per module." refs
  pure (overFunctions (const specs) run)
  where
    plans = modules (artefact run)
    spec (FunctionArg moduleName name signature doc change) = do
      let n = T.strip name
          m = T.strip moduleName
      plan <- case [p | p <- plans, moduleNameText (planModule p) == m] of
        p : _ -> Right p
        [] -> Left ("Unknown module " <> m <> " for " <> n <> ". Use one of the modules from SetTypes: " <> T.intercalate ", " (map (moduleNameText . planModule) plans))
      unless (validFunctionName n) $ Left ("Invalid function name: " <> n)
      let sig = stripPrefixes [n <> " ::", n <> "::", "::"] (T.strip signature)
      when (T.null sig) $ Left ("Missing signature for " <> n)
      let ref = FunctionRef (planPath plan) (FunctionName n)
      let before = if planIsNew plan then Nothing else previous ref
      pure (FunctionSpec ref Designed sig (T.strip doc) (T.strip change) Nothing (maybe Undefined (const PreviousBody) before) [] before)

applyProperties :: PropertiesArgs -> Run -> Either Text Run
applyProperties (PropertiesArgs importLines helpers props) run = do
  checkedImports <- traverse checkImport importLines
  checked <- traverse check props
  let names = map propName checked
  unique "Property names must be unique." names
  pure (overArtefact (\art -> art {propertyImports = checkedImports, propertyHelpers = helpers, properties = checked}) run)
  where
    check (PropertySpec name source) = do
      let n = T.strip name
      unless ("prop_" `T.isPrefixOf` n && validFunctionName n) $ Left ("Property names must start with prop_: " <> n)
      unless (any ((n `T.isPrefixOf`) . T.strip) (T.lines source)) $ Left ("The source of " <> n <> " must define " <> n <> ".")
      pure (PropertySpec n source)

-- | Sets one function's definition, dropping any type signature the model repeated.
applyImplementation :: FunctionRef -> ImplementArgs -> Run -> Either Text Run
applyImplementation ref (ImplementArgs definition importLines) run = do
  checkedImports <- traverse checkImport importLines
  body <- definitionOf (refName ref) definition
  unless (any ((== ref) . fnRef) (functions (artefact run))) $
    Left ("Unknown function " <> functionNameText (refName ref))
  let set f
        | fnRef f == ref = f {fnBody = Just body, fnImports = checkedImports}
        | otherwise = f
  pure (overFunctions (map set) run)

-- | A submitted definition, without any signature of the function the model
-- repeated: every equation of the function, or for an instance its whole
-- declaration. Anything else it defines at the top level would clash with
-- the rest of the module, so it is rejected.
definitionOf :: FunctionName -> Text -> Either Text Text
definitionOf name definition = do
  let n = functionNameText name
      body = T.unlines (filter (not . isSignature n) (T.lines definition))
  if isInstanceName name
    then unless (any (("instance " `T.isPrefixOf`) . T.stripStart) (T.lines definition)) $
      Left ("Give the complete declaration of " <> n <> ", starting with its instance head.")
    else unless (any (defines n . T.stripStart) (T.lines body)) $
      Left ("The definition must define " <> n <> ".")
  case nub [key | d <- sourceDecls (splitSource definition), Just key <- [declKey d], key /= n] of
    [] -> pure ()
    others -> Left ("Define only " <> n <> "; this also defines " <> T.intercalate ", " others <> ". Put helpers in a where clause.")
  pure (T.stripEnd (if isInstanceName name then definition else body))
  where
    isSignature n line = any (`T.isPrefixOf` T.strip line) [n <> " ::", n <> "::"]
    -- An equation of the function, prefix or infix.
    defines n line =
      n `T.isPrefixOf` line
        || ("`" <> n <> "`") `elem` infixPosition line
        || maybe False (`elem` infixPosition line) (T.stripPrefix "(" n >>= T.stripSuffix ")")
    infixPosition = take 1 . drop 1 . T.words

-- | What a review found the code does not do.
newtype GapArgs = GapArgs Text

instance FromJSON GapArgs where
  parseJSON = withObject "RequirementNotMet" $ \o -> GapArgs <$> o .: "reason"

newtype RefactorArgs = RefactorArgs [RefactorArg]

instance FromJSON RefactorArgs where
  parseJSON = withObject "Refactor" $ \o -> RefactorArgs <$> o .: "functions"

-- | A new definition for one of the run's functions, or a new helper, which
-- needs a signature.
data RefactorArg = RefactorArg
  { refactorModule :: Text,
    refactorName :: Text,
    refactorSignature :: Text,
    refactorDoc :: Text,
    refactorDefinition :: Text,
    refactorImports :: [Text]
  }

instance FromJSON RefactorArg where
  parseJSON = withObject "function" $ \o ->
    RefactorArg
      <$> o .: "module"
      <*> o .: "name"
      <*> o .:? "signature" .!= ""
      <*> o .:? "doc" .!= ""
      <*> o .: "definition"
      <*> o .:? "imports" .!= []

-- | Rewrites the run's functions and adds helpers, as a review asked.
applyRefactor :: RefactorArgs -> Run -> Either Text Run
applyRefactor (RefactorArgs args) run = do
  when (null args) $ Left "List at least one function, or reply without calling Refactor if nothing is duplicated."
  changes <- traverse change args
  unique "Change each function once." (map (fnRef . fst) changes)
  pure (overFunctions (\fns -> map (replaced changes) fns ++ [f | (f, True) <- changes]) run)
  where
    plans = modules (artefact run)
    existing = functions (artefact run)
    replaced changes f = case [c | (c, False) <- changes, fnRef c == fnRef f] of
      c : _ -> f {fnBody = fnBody c, fnImports = fnImports c, fnSignature = if T.null (fnSignature c) then fnSignature f else fnSignature c}
      [] -> f
    -- A function to replace, or a new one, said by the flag.
    change arg = do
      let n = T.strip (refactorName arg)
      plan <- case [p | p <- plans, moduleNameText (planModule p) == T.strip (refactorModule arg)] of
        p : _ -> Right p
        [] -> Left ("Unknown module " <> refactorModule arg <> ". Use one of: " <> T.intercalate ", " (map (moduleNameText . planModule) plans))
      unless (validFunctionName n) $ Left ("Invalid function name: " <> n)
      body <- definitionOf (FunctionName n) (refactorDefinition arg)
      imports' <- traverse checkImport (refactorImports arg)
      let ref = FunctionRef (planPath plan) (FunctionName n)
          signature = stripPrefixes [n <> " ::", n <> "::", "::"] (T.strip (refactorSignature arg))
          new = ref `notElem` map fnRef existing
      when (new && T.null signature) $ Left ("Give the new helper " <> n <> " a signature.")
      pure (FunctionSpec ref Designed signature (T.strip (refactorDoc arg)) "" (Just body) Undefined imports' Nothing, new)

-- | Adds existing functions the design broke, as holes, given each one's
-- declaration before the run. Functions already in the run are skipped.
addRepairs :: [(FunctionRef, Text)] -> Run -> Run
addRepairs broken = overFunctions (\fns -> fns ++ [repair ref previous | (ref, previous) <- broken, ref `notElem` map fnRef fns])
  where
    repair ref previous = FunctionSpec ref Repair "" "" "" Nothing Undefined [] (Just previous)

-- | Fails with the message if any item repeats.
unique :: (Eq a) => Text -> [a] -> Either Text ()
unique message xs = when (length (nub xs) /= length xs) (Left message)

checkImport :: Text -> Either Text Text
checkImport line
  | "import " `T.isPrefixOf` T.strip line = Right (T.strip line)
  | otherwise = Left ("Not an import line: " <> line)

validFunctionName :: Text -> Bool
validFunctionName name = case T.uncons name of
  Just (c, rest) -> (isLower c || c == '_') && T.all isIdentChar rest
  Nothing -> False

stripPrefixes :: [Text] -> Text -> Text
stripPrefixes prefixes value = case [rest | p <- prefixes, Just rest <- [T.stripPrefix p value]] of
  rest : _ -> T.strip rest
  [] -> value
