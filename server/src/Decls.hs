{-# LANGUAGE OverloadedStrings #-}

-- | Haskell source split by layout into a header (pragmas, module header and
-- imports) and top-level declarations. A declaration starts at a column-0
-- line of code, takes the comments and pragmas directly above it, and runs
-- to the next one; a signature and its equations are one declaration.
-- Joining the parts gives back the source exactly, so an edit leaves every
-- other line as it was.
module Decls
  ( SourceFile (..),
    Decl (..),
    DeclKind (..),
    splitSource,
    joinSource,
    declSpans,
    declAt,
    findDecl,
    importAt,
    upsertDecl,
    rewriteEquations,
    instanceHeadLines,
    rewriteInstance,
    adaptEquations,
    parameters,
    exportName,
    exportedNames,
    addPragmas,
    addImports,
    addExports,
  )
where

import Data.Char (isAlphaNum, isLower, isSpace)
import Data.List (findIndex, isPrefixOf)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

data SourceFile = SourceFile
  { sourceHeader :: [Text],
    sourceDecls :: [Decl]
  }
  deriving (Eq, Show)

data Decl = Decl
  { declKind :: DeclKind,
    -- | The name the declaration defines: a function, type or class name, an
    -- operator in parentheses, or @instance <head>@. 'Nothing' for splices
    -- and other declarations that define no single name.
    declKey :: Maybe Text,
    declLines :: [Text],
    -- | For a function, the lines before its first equation: comments,
    -- signature and pragmas.
    declHeadLength :: Int
  }
  deriving (Eq, Show)

data DeclKind
  = ValueDecl
  | -- | @data@, @newtype@ and @class@, exported with their constructors or methods.
    DataDecl
  | -- | Type synonyms and families.
    TypeDecl
  | InstanceDecl
  | OtherDecl
  deriving (Eq, Show)

data LineKind = Blank | Indented | Comment | Pragma | HeaderLine | Code
  deriving (Eq, Show)

splitSource :: Text -> SourceFile
splitSource source = SourceFile (take start ls) (mergeChunks (map describe (slices bounds)))
  where
    ls = T.lines source
    kinds = classify ls
    -- A chunk starts at a column-0 line of code, or at the comments and
    -- pragmas directly above it.
    bounds = [attached i | (i, Code) <- zip [0 ..] kinds]
    attached i = i - length (takeWhile (`elem` [Comment, Pragma]) (reverse (take i kinds)))
    start = case bounds of
      first : _ -> first
      [] -> length ls
    slices bs = [take (to - from) (drop from (zip ls kinds)) | (from, to) <- zip bs (drop 1 bs ++ [length ls])]

joinSource :: SourceFile -> Text
joinSource sf = T.unlines (sourceHeader sf ++ concatMap declLines (sourceDecls sf))

-- | Each declaration with its first and last line, 1-based and inclusive.
declSpans :: SourceFile -> [(Decl, (Int, Int))]
declSpans sf = zip (sourceDecls sf) (zip starts (map (subtract 1) (drop 1 starts)))
  where
    starts = scanl (+) (length (sourceHeader sf) + 1) (map (length . declLines) (sourceDecls sf))

declAt :: Int -> SourceFile -> Maybe Decl
declAt line sf = listToMaybe [d | (d, (from, to)) <- declSpans sf, line >= from, line <= to]

findDecl :: Text -> SourceFile -> Maybe Decl
findDecl key sf = listToMaybe [d | d <- sourceDecls sf, declKey d == Just key]

-- | The import line starting at a header line, with its whitespace collapsed.
importAt :: Int -> SourceFile -> Maybe Text
importAt line sf = case drop (line - 1) (sourceHeader sf) of
  l : _ | "import " `T.isPrefixOf` l -> Just (collapse l)
  _ -> Nothing

-- | Replaces the declaration with the same key, keeping the blank lines and
-- comments after it, or adds the declaration after the others.
upsertDecl :: Decl -> SourceFile -> SourceFile
upsertDecl new sf = case break sameKey (sourceDecls sf) of
  (before, old : after) ->
    sf {sourceDecls = before ++ new {declLines = declLines new ++ trailing old} : after}
  (_, []) -> case sourceDecls sf of
    [] -> sf {sourceHeader = separated (sourceHeader sf), sourceDecls = [new]}
    ds -> sf {sourceDecls = init ds ++ [separatedDecl (last ds), new]}
  where
    sameKey d = isJust (declKey new) && declKey d == declKey new
    separated ls = if null ls || T.null (T.strip (last ls)) then ls else ls ++ [""]
    separatedDecl d = d {declLines = separated (declLines d)}

-- | Replaces a function's equations, keeping its comments, signature and
-- pragmas and anything after it. 'Nothing' leaves a hole: @name = undefined@.
rewriteEquations :: Text -> Maybe Text -> Decl -> Decl
rewriteEquations name body d =
  d {declLines = take (min (declHeadLength d) code) (declLines d) ++ equations ++ after}
  where
    after = trailing d
    code = length (declLines d) - length after
    equations = maybe [name <> " = undefined"] (T.lines . T.stripEnd) body

-- | An instance's comments and head, up to its @where@, without its methods.
-- 'Nothing' for an instance without methods.
instanceHeadLines :: Decl -> Maybe [Text]
instanceHeadLines d
  | declKind d /= InstanceDecl = Nothing
  | otherwise = case break endsHead (declLines d) of
      (before, end : _) -> Just (before ++ [end])
      (_, []) -> Nothing
  where
    endsHead l = case reverse (T.words (fst (T.breakOn "--" l))) of
      "where" : _ -> True
      _ -> False

-- | Replaces an instance with a new declaration, or with just its head as a
-- hole while it is repaired, keeping the lines after it.
rewriteInstance :: Maybe Text -> Decl -> Maybe Decl
rewriteInstance body d = do
  replacement <- maybe (instanceHeadLines d) (Just . T.lines . T.stripEnd) body
  pure d {declLines = replacement ++ trailing d}

-- | A function's equations adapted to a new signature, to stand in for it
-- until it is reimplemented: as they are when the signature has as many
-- parameters as before, or with a wildcard pattern for each new parameter
-- when the old parameters appear in the new signature in order. 'Nothing'
-- when that cannot be done: the function has no signature or is defined
-- infix, a parameter was dropped, or an equation binds fewer patterns than
-- the function has parameters.
adaptEquations :: Text -> Text -> Decl -> Maybe [Text]
adaptEquations name newSignature d = do
  old <- signatureOf name d
  let before = parameters old
      after = parameters newSignature
  wildcards <- case compare (length after) (length before) of
    EQ -> Just []
    GT | not (isOperatorName name) -> newPositions before (zip [0 ..] after)
    _ -> Nothing
  if null wildcards then Just equations else traverse (adapt (length before) (length after) wildcards) equations
  where
    after' = trailing d
    equations = take (length (declLines d) - length after' - declHeadLength d) (drop (declHeadLength d) (declLines d))
    adapt arity total wildcards l = case T.stripPrefix name l of
      Just rest | T.null rest || isSpace (T.head rest) -> do
        let (patterns, body) = splitEquation rest
            tokens = patternTokens patterns
        if length tokens /= arity
          then Nothing
          else Just (T.unwords (name : place total wildcards tokens) <> (if T.null (T.strip body) then "" else " " <> T.stripStart body))
      _ -> Just l
    place total wildcards = go 0
      where
        go i tokens
          | i >= total = []
          | i `elem` wildcards = "_" : go (i + 1) tokens
          | otherwise = case tokens of
              t : rest -> t : go (i + 1) rest
              [] -> []

-- | The positions in the new parameters of those that are not old ones,
-- matching the old ones in order.
newPositions :: [Text] -> [(Int, Text)] -> Maybe [Int]
newPositions [] news = Just (map fst news)
newPositions _ [] = Nothing
newPositions (o : os) ((i, n) : ns)
  | o == n = newPositions os ns
  | otherwise = (i :) <$> newPositions (o : os) ns

-- | The type of a function's signature in its declaration, without comments.
signatureOf :: Text -> Decl -> Maybe Text
signatureOf name d = case break isSignature headLines of
  (_, first : rest) ->
    let continued = takeWhile (\l -> not (T.null l) && isSpace (T.head l)) rest
     in Just (T.drop 2 (snd (T.breakOn "::" (T.unwords (map withoutComment (first : continued))))))
  (_, []) -> Nothing
  where
    headLines = take (declHeadLength d) (declLines d)
    isSignature l = maybe False (name `elem`) (signatureNames (collapse l))
    withoutComment l = case T.breakOn "--" l of
      (before, rest) | not (T.null rest), lineComment rest -> before
      _ -> l

-- | The parameter types of a signature, after any @forall@ and context.
parameters :: Text -> [Text]
parameters signature = map collapse (dropLast (splitArrows (afterContext (afterForall (T.strip signature)))))
  where
    afterForall t
      | "forall" `T.isPrefixOf` t = T.strip (T.drop 1 (snd (T.breakOn "." t)))
      | otherwise = t
    dropLast xs = take (length xs - 1) xs

-- | A type split at its top-level arrows.
splitArrows :: Text -> [Text]
splitArrows = map (T.strip . T.pack) . go (0 :: Int) "" . T.unpack
  where
    go _ acc [] = [reverse acc]
    go depth acc ('-' : '>' : rest) | depth == 0 = reverse acc : go depth "" rest
    go depth acc (c : rest)
      | c `elem` ("([{" :: String) = go (depth + 1) (c : acc) rest
      | c `elem` (")]}" :: String) = go (depth - 1) (c : acc) rest
      | otherwise = go depth (c : acc) rest

-- | An equation's patterns and the rest from its @=@ or first guard.
splitEquation :: Text -> (Text, Text)
splitEquation t = T.splitAt (go (0 :: Int) 0 (T.unpack t)) t
  where
    go _ i [] = i
    go depth i (c : rest)
      | c `elem` ("([{" :: String) = go (depth + 1) (i + 1) rest
      | c `elem` (")]}" :: String) = go (depth - 1) (i + 1) rest
      | c == '"' = let n = stringLength rest in go depth (i + 1 + n) (drop n rest)
      | depth == 0, c `elem` ("=|" :: String), not (symbolAround i) = i
      | otherwise = go depth (i + 1) rest
    symbolAround i = any isSymbol [ch | j <- [i - 1, i + 1], j >= 0, j < T.length t, let ch = T.index t j]

-- | An equation's patterns, split at top-level spaces.
patternTokens :: Text -> [Text]
patternTokens = filter (not . T.null) . map (T.strip . T.pack) . go (0 :: Int) "" . T.unpack
  where
    go _ acc [] = [reverse acc]
    go depth acc (c : rest)
      | c `elem` ("([{" :: String) = go (depth + 1) (c : acc) rest
      | c `elem` (")]}" :: String) = go (depth - 1) (c : acc) rest
      | c == '"' = let n = stringLength rest in go depth (reverse (take n rest) ++ c : acc) (drop n rest)
      | depth == 0 && isSpace c = reverse acc : go depth "" rest
      | otherwise = go depth (c : acc) rest

-- | The length of a string literal's rest, up to and including its closing quote.
stringLength :: String -> Int
stringLength = go 0
  where
    go n ('\\' : _ : rest) = go (n + 2) rest
    go n ('"' : _) = n + 1
    go n (_ : rest) = go (n + 1) rest
    go n [] = n

-- | How a declaration is exported, if it defines a name.
exportName :: Decl -> Maybe Text
exportName d = case (declKind d, declKey d) of
  (DataDecl, Just key) -> Just (key <> " (..)")
  (TypeDecl, Just key) -> Just key
  (ValueDecl, Just key) -> Just key
  _ -> Nothing

-- | Adds LANGUAGE pragmas the file does not already enable, at the top.
addPragmas :: [Text] -> SourceFile -> SourceFile
addPragmas extensions sf = sf {sourceHeader = ["{-# LANGUAGE " <> e <> " #-}" | e <- missing] ++ sourceHeader sf}
  where
    enabled = concat [map T.strip (T.splitOn "," (pragmaBody l)) | l <- sourceHeader sf, "LANGUAGE" `T.isInfixOf` l]
    pragmaBody = T.replace "#-}" "" . snd . T.breakOnEnd "LANGUAGE"
    missing = dedupe [T.strip e | e <- extensions, T.strip e `notElem` enabled]

-- | Adds import lines the file does not already have, after its last import.
addImports :: [Text] -> SourceFile -> SourceFile
addImports imports sf
  | null missing = sf
  | otherwise = sf {sourceHeader = take at hs ++ spacer ++ missing ++ drop at hs}
  where
    hs = sourceHeader sf
    existing = [collapse l | l <- hs, "import " `T.isPrefixOf` l]
    missing = dedupe [collapse i | i <- imports, collapse i `notElem` existing]
    lastImport = listToMaybe (reverse [i | (i, l) <- zip [0 ..] hs, "import " `T.isPrefixOf` l])
    at = case lastImport of
      Just i -> i + 1 + length (takeWhile continues (drop (i + 1) hs))
      Nothing -> maybe 0 (+ 1) moduleEnd
    continues l = not (T.null l) && isSpace (T.head l)
    -- The line with the @where@ that ends the module header.
    moduleEnd = do
      m <- findIndex ("module" `T.isPrefixOf`) hs
      (+ m) <$> findIndex (elem "where" . T.words) (drop m hs)
    spacer = if isJust lastImport || at == 0 || T.null (T.strip (hs !! (at - 1))) then [] else [""]

-- | The items of the module's export list, or 'Nothing' if it has none.
exportedNames :: SourceFile -> Maybe [Text]
exportedNames sf = do
  (open, _, close) <- exportListEnd s
  pure . filter (not . T.null) . map T.strip . splitTop $ withoutComments (take (close - open - 1) (drop (open + 1) s))
  where
    s = T.unpack (T.intercalate "\n" (sourceHeader sf))
    withoutComments = unlines . map (T.unpack . dropComment . T.pack) . lines
    -- A line without its @--@ comment; @-->@ and other operators stay.
    dropComment t = case T.breakOn "--" t of
      (before, rest)
        | T.null rest -> t
        | lineComment rest -> before
        | otherwise -> before <> "--" <> dropComment (T.drop 2 rest)
    splitTop = go (0 :: Int) ""
    go _ acc [] = [T.pack (reverse acc)]
    go depth acc (c : cs)
      | c == '(' = go (depth + 1) (c : acc) cs
      | c == ')' = go (depth - 1) (c : acc) cs
      | c == ',' && depth == 0 = T.pack (reverse acc) : go depth "" cs
      | otherwise = go depth (c : acc) cs

-- | Adds names to the module's export list, if it has one and does not
-- already export them. A module without an export list already exports
-- everything.
addExports :: [Text] -> SourceFile -> SourceFile
addExports wanted sf = case (names, exportListEnd (T.unpack text)) of
  ([], _) -> sf
  (_, Nothing) -> sf
  (_, Just (_, lastChar, _)) ->
    let (before, after) = splitAt (lastChar + 1) (T.unpack text)
        line = T.splitOn "\n" text !! length (filter (== '\n') before)
        inserted = case T.unpack text !! lastChar of
          '(' -> T.intercalate ", " names
          ','
            | "module" `T.isPrefixOf` line -> " " <> T.intercalate ", " names <> ","
            | otherwise -> T.concat ["\n" <> T.takeWhile isSpace line <> n <> "," | n <- names]
          _ -> ", " <> T.intercalate ", " names
     in sf {sourceHeader = T.splitOn "\n" (T.pack before <> inserted <> T.pack after)}
  where
    text = T.intercalate "\n" (sourceHeader sf)
    base = T.takeWhile (/= ' ') . T.strip
    present = maybe [] (map base) (exportedNames sf)
    names = dedupe [n | n <- wanted, base n `notElem` present]

-- | The positions of the export list's opening parenthesis, of the last
-- character before its closing parenthesis that is not whitespace or part
-- of a comment, and of the closing parenthesis.
exportListEnd :: String -> Maybe (Int, Int, Int)
exportListEnd s = do
  m <- findModule 0 s
  let afterName = skipName (m + length ("module" :: String))
  open <- case skipBlank afterName of
    i | i < length s && s !! i == '(' -> Just i
    _ -> Nothing
  (lastSig, close) <- scan (open + 1) (1 :: Int) open
  pure (open, lastSig, close)
  where
    findModule i rest
      | "module" `isPrefixOf` rest && (i == 0 || s !! (i - 1) == '\n') = Just i
      | otherwise = case rest of
          _ : more -> findModule (i + 1) more
          [] -> Nothing
    skipName i = let j = skipBlank i in j + length (takeWhile (\c -> isAlphaNum c || c `elem` ("._'" :: String)) (drop j s))
    skipBlank i = case drop i s of
      c : _ | isSpace c -> skipBlank (i + 1)
      rest@('-' : '-' : _) | lineComment (T.pack (takeWhile (/= '\n') rest)) -> skipBlank (i + length (takeWhile (/= '\n') rest))
      '{' : '-' : _ -> skipBlank (skipBlock (i + 2) 1)
      _ -> i
    skipBlock i depth = case drop i s of
      _ | depth == (0 :: Int) -> i
      '{' : '-' : _ -> skipBlock (i + 2) (depth + 1)
      '-' : '}' : _ -> skipBlock (i + 2) (depth - 1)
      _ : _ -> skipBlock (i + 1) depth
      [] -> i
    scan i depth lastSig = case drop i s of
      [] -> Nothing
      _ | skipBlank i > i -> scan (skipBlank i) depth lastSig
      '(' : _ -> scan (i + 1) (depth + 1) i
      ')' : _
        | depth == 1 -> Just (lastSig, i)
        | otherwise -> scan (i + 1) (depth - 1) i
      _ -> scan (i + 1) depth i

-- Classifying lines

classify :: [Text] -> [LineKind]
classify = go Nothing
  where
    -- Inside a multi-line block comment or pragma: its kind and nesting depth.
    go _ [] = []
    go (Just (kind, depth)) (l : rest) =
      let depth' = depth + opens l - closes l
       in kind : go (if depth' > 0 then Just (kind, depth') else Nothing) rest
    go Nothing (l : rest) =
      let kind = lineKind l
          depth = opens l - closes l
          open = kind `elem` [Comment, Pragma] && "{-" `T.isPrefixOf` l && depth > 0
       in kind : go (if open then Just (kind, depth) else Nothing) rest
    opens = T.count "{-"
    closes = T.count "-}"

lineKind :: Text -> LineKind
lineKind l
  | T.null (T.strip l) = Blank
  | isSpace (T.head l) = Indented
  | "{-#" `T.isPrefixOf` l = Pragma
  | "{-" `T.isPrefixOf` l = Comment
  | "#" `T.isPrefixOf` l = Pragma
  | lineComment l = Comment
  | otherwise = case T.words l of
      w : _ | w `elem` ["module", "import", "where"] -> HeaderLine
      _ -> Code

lineComment :: Text -> Bool
lineComment l = case T.stripPrefix "--" l of
  Just rest -> case T.uncons (T.dropWhile (== '-') rest) of
    Just (c, _) -> not (isSymbol c)
    Nothing -> True
  Nothing -> False

isSymbol :: Char -> Bool
isSymbol c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

-- Describing chunks

-- | A chunk: the comments above a column-0 line of code, the line, and
-- everything up to the next chunk.
describe :: [(Text, LineKind)] -> Decl
describe chunkLines = Decl kind key (map fst chunkLines) headLength
  where
    codeAt = findIndex ((== Code) . snd) chunkLines
    codeText = case codeAt of
      Just i ->
        collapse . T.unwords $
          fst (chunkLines !! i) : [l | (l, Indented) <- takeWhile ((`elem` [Indented, Blank]) . snd) (drop (i + 1) chunkLines)]
      Nothing -> ""
    (kind, key, isEquation) = codeInfo codeText
    headLength = case codeAt of
      Just i | isEquation -> i
      _ -> length chunkLines

-- | Joins a signature with the equations after it, and the equations of one
-- function, into one declaration.
mergeChunks :: [Decl] -> [Decl]
mergeChunks (a : b : rest)
  | isJust (declKey a) && declKey a == declKey b && declKind a == declKind b =
      mergeChunks (a {declLines = declLines a ++ declLines b, declHeadLength = headLength} : rest)
  where
    headLength
      | declHeadLength a < length (declLines a) = declHeadLength a
      | otherwise = length (declLines a) + declHeadLength b
mergeChunks (a : rest) = a : mergeChunks rest
mergeChunks [] = []

-- | The kind and key of a declaration from its first line of code and the
-- indented lines after it, and whether that line starts an equation.
codeInfo :: Text -> (DeclKind, Maybe Text, Bool)
codeInfo code = case T.words code of
  "data" : "family" : rest -> (TypeDecl, typeName rest, False)
  "data" : "instance" : _ -> (InstanceDecl, Just ("data " <> instanceHead code), False)
  "data" : rest -> (DataDecl, typeName rest, False)
  "newtype" : "instance" : _ -> (InstanceDecl, Just ("newtype " <> instanceHead code), False)
  "newtype" : rest -> (DataDecl, typeName rest, False)
  "type" : "family" : rest -> (TypeDecl, typeName rest, False)
  "type" : "instance" : _ -> (InstanceDecl, Just ("type " <> instanceHead code), False)
  "type" : rest -> (TypeDecl, typeName rest, False)
  "class" : rest -> (DataDecl, typeName rest, False)
  "instance" : _ -> (InstanceDecl, Just ("instance " <> instanceHead code), False)
  "deriving" : _ | "instance" `elem` T.words code -> (InstanceDecl, Just ("instance " <> instanceHead code), False)
  w : _ | w `elem` ["infix", "infixl", "infixr", "foreign", "default", "pattern"] -> (OtherDecl, Nothing, False)
  _ -> case signatureNames code of
    Just (name : _) -> (ValueDecl, Just name, False)
    _ -> case equationName code of
      Just name -> (ValueDecl, Just name, True)
      Nothing -> (OtherDecl, Nothing, False)

-- | The declared type or class name, after any context.
typeName :: [Text] -> Maybe Text
typeName ws = case T.words (afterContext (T.unwords (takeWhile (`notElem` ["=", "where", "::"]) ws))) of
  w : _ -> case T.filter (`notElem` ("()" :: String)) w of
    name | not (T.null name) -> Just name
    _ -> Nothing
  [] -> Nothing

-- | The instance head after @instance@ and any context, without @where@.
instanceHead :: Text -> Text
instanceHead code = collapse (afterContext (T.strip (stripWhere (snd (T.breakOnEnd "instance " code)))))
  where
    stripWhere t = case T.breakOn " where" (t <> " ") of
      (before, rest) | not (T.null rest) -> before
      _ -> t

afterContext :: Text -> Text
afterContext t = case T.breakOn "=>" t of
  (_, rest) | not (T.null rest) -> T.strip (T.drop 2 rest)
  _ -> t

-- | The names in a type signature: @f, g :: ...@ or @(<+>) :: ...@.
signatureNames :: Text -> Maybe [Text]
signatureNames code = case T.breakOn "::" code of
  (before, rest)
    | not (T.null rest),
      names <- map T.strip (T.splitOn "," before),
      not (null names),
      all (\n -> isVariable n || isOperatorName n) names ->
        Just names
  _ -> Nothing

-- | The function an equation defines: @f x = ...@, @x <+> y = ...@,
-- @x \`f\` y = ...@ or @(<+>) x y = ...@.
equationName :: Text -> Maybe Text
equationName code
  | not (any (`elem` ["=", "|"]) ws) = Nothing
  | otherwise = case ws of
      first : second : _
        | isVariable first && T.all isSymbol second && second `notElem` ["=", "|", "@", "::"] -> Just ("(" <> second <> ")")
        | isVariable first, Just name <- T.stripPrefix "`" second >>= T.stripSuffix "`" -> Just name
      first : _
        | isVariable first -> Just first
        | Just op <- T.stripPrefix "(" first >>= T.stripSuffix ")", T.all isSymbol op, not (T.null op) -> Just first
      _ -> Nothing
  where
    ws = T.words code

isVariable :: Text -> Bool
isVariable name = case T.uncons name of
  Just (c, rest) -> (isLower c || c == '_') && T.all (\x -> isAlphaNum x || x == '_' || x == '\'') rest && name `notElem` keywords
  Nothing -> False
  where
    keywords = ["case", "do", "if", "let", "of", "then", "else", "in", "where"]

isOperatorName :: Text -> Bool
isOperatorName name = case T.stripPrefix "(" name >>= T.stripSuffix ")" of
  Just op -> not (T.null op) && T.all isSymbol op
  Nothing -> False

-- Helpers

-- | The blank lines and column-0 comments after a declaration's last line of code.
trailing :: Decl -> [Text]
trailing d = reverse (takeWhile loose (reverse (declLines d)))
  where
    loose l = lineKind l `elem` [Blank, Comment]

collapse :: Text -> Text
collapse = T.unwords . T.words

dedupe :: [Text] -> [Text]
dedupe = foldr (\x acc -> x : filter (/= x) acc) []
