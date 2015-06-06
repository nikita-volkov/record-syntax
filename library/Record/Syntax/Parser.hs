module Record.Syntax.Parser where

import Record.Syntax.Prelude hiding (try, takeWhile)
import Record.Syntax.Shared
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P
import qualified Data.Text.Lazy.Builder as TLB
import qualified Record.Syntax.Renderer as Renderer
import qualified Record.Syntax.Position as Position


newtype Parser a =
  Parser (P.Parsec Text () a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance Monoid a => Monoid (Parser a) where
  mempty = 
    pure mempty
  mappend (Parser a) (Parser b) = 
    Parser $ liftA2 mappend a b

run :: Parser a -> Text -> Either Error a
run (Parser p) =
  either (Left . convertError) Right .
  P.parse p ""
  where
    convertError =
      (,) <$> Position.fromParsec . P.errorPos <*> intercalate "; " . fmap P.messageString . P.errorMessages


-- * Combinators
-------------------------

lookAhead :: Parser a -> Parser a
lookAhead (Parser p) =
  Parser $ P.lookAhead p

try :: Parser a -> Parser a
try (Parser p) =
  Parser $ P.try p

labeled :: String -> Parser a -> Parser a
labeled label (Parser p) =
  Parser $ P.label p label

total :: Parser a -> Parser a
total (Parser p) =
  Parser $ p <* P.eof

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill (Parser a) (Parser b) =
  Parser $ P.manyTill a b

manyTillPair :: Parser a -> Parser b -> Parser ([a], b)
manyTillPair a b =
  fix $ \loop -> 
    ([],) <$> b <|> 
    (\a (al, b) -> (a : al, b)) <$> a <*> loop

manyTillMonoid :: Monoid a => Parser a -> Parser a -> Parser a
manyTillMonoid a b =
  fmap (\(c, d) -> mconcat c <> d) $
  manyTillPair a b

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy (Parser a) (Parser b) =
  Parser $ P.sepBy a b

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 (Parser a) (Parser b) =
  Parser $ P.sepBy1 a b

manyMerged :: Parser TextBuilder -> Parser TextBuilder
manyMerged =
  fmap mconcat . many


-- * General
-------------------------

char :: Char -> Parser TextBuilder
char x =
  Parser $ P.char x $> TLB.singleton x

string :: String -> Parser TextBuilder
string x =
  Parser $ P.string x $> TLB.fromString x

space :: Parser TextBuilder
space =
  satisfy isSpace

spaces :: Parser TextBuilder
spaces =
  Parser $ TLB.fromString <$> many (P.satisfy isSpace)

spaces1 :: Parser TextBuilder
spaces1 =
  Parser $ TLB.fromString <$> P.many1 (P.satisfy isSpace)

nonEOLSpace :: Parser TextBuilder
nonEOLSpace =
  satisfy ((&&) <$> isSpace <*> not . flip elem ['\n', '\r'])

nonEOLSpaces :: Parser TextBuilder
nonEOLSpaces =
  manyMerged nonEOLSpace

satisfy :: (Char -> Bool) -> Parser TextBuilder
satisfy p =
  Parser $ TLB.singleton <$> P.satisfy p

takeWhile :: (Char -> Bool) -> Parser TextBuilder
takeWhile p =
  Parser $ TLB.fromString <$> P.many (P.satisfy p)

takeWhile1 :: (Char -> Bool) -> Parser TextBuilder
takeWhile1 p =
  Parser $ TLB.fromString <$> P.many1 (P.satisfy p)

anyChar :: Parser TextBuilder
anyChar =
  Parser $ TLB.singleton <$> P.anyChar

endOfLine :: Parser TextBuilder
endOfLine =
  string "\r\n" <|> char '\r' <|> char '\n'

endOfFile :: Parser TextBuilder
endOfFile =
  Parser $ P.eof $> mempty

noneOf :: [Char] -> Parser TextBuilder
noneOf =
  Parser . fmap TLB.singleton . P.noneOf


-- * Domain-specific
-------------------------

position :: Parser Position.Position
position =
  Position.fromParsec <$> Parser P.getPosition

unparsedExtensionLexeme :: Parser TextBuilder
unparsedExtensionLexeme =
  unparsedRecordBlock True <|> unparsedRecordBlock False <|> unparsedLabel True <|> unparsedLabel False

unparsedRecordBlock :: Bool -> Parser TextBuilder
unparsedRecordBlock =
  block . bool lazyDelimiters strictDelimiters
  where
    strictDelimiters =
      (try (string "{!"), try (string "!}" <|> string "}"))
    lazyDelimiters =
      (try (string "{~"), try (string "~}" <|> string "}"))
    block (opening, closing) =
      opening <> manyTillMonoid plainTree closing

unparsedLabel :: Bool -> Parser TextBuilder
unparsedLabel uppercase =
  try (char '@' <> identifier uppercase)

recordExp :: Bool -> Parser (RecordExp (Position, TextBuilder))
recordExp strict =
  fmap RecordExp . (,) <$> pure strict <*> decls (bool lazyDelimiters strictDelimiters strict)
  where
    strictDelimiters =
      (try (string "{!"), try (spaces *> (string "!}" <|> string "}")))
    lazyDelimiters =
      (try (string "{~"), try (spaces *> (string "~}" <|> string "}")))
    decls (opening, closing) =
      opening *> spaces *> sepBy1 (assignment <|> nonAssignment) sep <* closing
      where
        sep =
          try $ spaces *> char ',' *> spaces
        assignment =
          (,) <$> try (identifier False <* spaces <* char '=' <* spaces) <*> 
                  (fmap Just . (,) <$> position <*> value)
          where
            value =
              fmap mconcat $
              manyTill plainTree (lookAhead (void sep <|> void closing))
        nonAssignment =
          (,) <$> identifier False <*> pure Nothing

labelExp :: Parser Label
labelExp =
  char '@' *> (identifier False <|> identifier True)

extensionExp :: Parser (ExtensionExp (Position, TextBuilder))
extensionExp =
  ExtensionExp_Record <$> (recordExp True <|> recordExp False) <|>
  ExtensionExp_Label <$> labelExp

extensionType :: Parser ExtensionType
extensionType =
  ExtensionType_Record <$> (recordType True <|> recordType False)

recordType :: Bool -> Parser RecordType
recordType strict =
  (,) <$> pure strict <*> decls (bool lazyDelimiters strictDelimiters strict)
  where
    strictDelimiters =
      (try (string "{!"), try (spaces *> (string "!}" <|> string "}")))
    lazyDelimiters =
      (try (string "{~"), try (spaces *> (string "~}" <|> string "}")))
    decls (opening, closing) =
      opening *> spaces *> sepBy1 decl sep <* closing
      where
        decl =
          (,) <$> try (identifier False <* spaces <* string "::" <* spaces) <*> type_
          where
            type_ =
              fmap ExtendableSyntaxForest $
              manyTill (extendableSyntaxTree extensionType) (lookAhead (void sep <|> void closing))
        sep =
          try $ spaces *> char ',' *> spaces

type ModuleHead =
  (TextBuilder, LineType)

moduleHead :: Parser ModuleHead
moduleHead =
  labeled "moduleHead" $
    (,) <$> text <*> lookAheadLineType
  where
    text =
      spaces <> fmap mconcat (sepBy (pragma <|> blockComment <|> inlineComment) spaces) <> moduleDeclaration <>
      nonEOLSpaces <> (endOfLine <|> endOfFile)

data LineType =
  LineType_Space |
  LineType_Comment |
  LineType_Import |
  LineType_Other

lookAheadLineType :: Parser LineType
lookAheadLineType =
  lookAhead $
    LineType_Space <$ try (nonEOLSpaces *> (endOfLine <|> endOfFile)) <|>
    LineType_Comment <$ try (nonEOLSpaces *> (inlineComment <|> blockComment)) <|>
    LineType_Import <$ importStatement <|>
    LineType_Other <$ pure ()

moduleDeclaration :: Parser TextBuilder
moduleDeclaration =
  labeled "moduleDeclaration" $
    try (string "module" <> spaces1) <> 
    qualifiedIdentifier True <> 
    (try (spaces <> exportsBlock <> spaces) <|> spaces1) <>
    string "where"
  where
    exportsBlock =
      Renderer.recursiveBlock (const mempty) <$>
      recursiveBlock empty BraceType_Round

importStatement :: Parser TextBuilder
importStatement =
  labeled "importStatement" $
    try (string "import" <> spaces1) <>
    fmap (fromMaybe mempty) (try (optional (string "qualified" <> spaces1))) <>
    qualifiedIdentifier True

qualifiedIdentifier :: Bool -> Parser TextBuilder
qualifiedIdentifier uppercase =
  labeled "qualifiedIdentifier" $
    try ((try (manyMerged (identifier True <> char '.')) <|> mempty) <> identifier uppercase)

identifier :: Bool -> Parser TextBuilder
identifier uppercase =
  labeled "identifier" $
    try (satisfy (\c -> bool isLower isUpper uppercase c || c == '_' || c == '\'')) <>
    fmap mconcat (many bodyChar)
  where
    bodyChar = satisfy (flip any [isAlphaNum, (== '\''), (== '_')] . flip ($))

quasiQuote :: Parser TextBuilder
quasiQuote =
  labeled "quasiQuote" $
    try (char '[' <> qualifiedIdentifier False <> char '|') <> 
    manyTillMonoid anyChar (string "|]")

inlineComment :: Parser TextBuilder
inlineComment =
  labeled "inlineComment" $
    try (string "--") <> 
    fmap mconcat (manyTill anyChar (lookAhead (endOfLine <|> endOfFile))) <>
    (endOfLine <|> pure mempty)

blockComment :: Parser TextBuilder
blockComment =
  labeled "blockComment" $
    try (string "{-") <>
    fmap mconcat (manyTill anyChar (string "-}")) <>
    pure "-}"

pragma :: Parser TextBuilder
pragma =
  labeled "pragma" $
    try (string "{-#") <> manyTillMonoid anyChar (string "#-}")

charLit :: Parser TextBuilder
charLit =
  labeled "charLit" $ try $
    char '\'' <>
    (escapeSequence <|> noneOf "'\\") <>
    char '\''
  where
    escapeSequence =
      char '\\' <> (char '\'' <|> takeWhile1 isSequenceChar)
      where
        isSequenceChar =
          \c -> c /= '\'' && c /= '\\' && not (isSpace c)

stringLit :: Parser TextBuilder
stringLit =
  labeled "stringLit" $
    try (char '"') <> manyTillMonoid (escapedChar <|> anyChar) (char '"')
  where
    escapedChar =
      char '\\' <> anyChar

plainLexeme :: Parser TextBuilder
plainLexeme =
  charLit <|> stringLit <|> quasiQuote <|>
  blockComment <|> inlineComment

plainRecursiveSyntax :: Parser TextBuilder
plainRecursiveSyntax =
  labeled "plainRecursiveSyntax" $
  fmap mconcat $ many $
    plainLexeme <|>
    label True <|>
    label False <|>
    record True <|>
    record False <|>
    block <|>
    noneOf "[{()}]"
  where
    label uppercase =
      char '@' <> identifier uppercase
    record =
      recordBraces >>> \(i, o) -> 
        try (string i) <> fmap mconcat (many plainRecursiveSyntax) <> string o
      where
        recordBraces =
          bool ("{~", "~}") ("{!", "!}")
    block =
      msum $ flip map [('{', '}'), ('(', ')'), ('[', ']')] $ \(i, o) ->
        char i <> plainRecursiveSyntax <> char o

extendableSyntaxForest :: Parser a -> Parser (ExtendableSyntaxForest a)
extendableSyntaxForest p =
  fmap ExtendableSyntaxForest $ many $ extendableSyntaxTree p

extendableSyntaxTree :: Parser a -> Parser (ExtendableSyntaxTree a)
extendableSyntaxTree p =
  labeled "extendableSyntaxTree" $
    fmap ExtendableSyntaxTree_Extension p <|>
    fmap ExtendableSyntaxTree_RecursiveBlock recursiveBlockVariations <|>
    fmap ExtendableSyntaxTree_Lexeme lexeme
  where
    recursiveBlockVariations =
      msum (map (recursiveBlock p) [minBound .. maxBound])
    lexeme =
      charLit <|> stringLit <|> quasiQuote <|>
      blockComment <|> inlineComment <|> 
      satisfy (not . flip elem ("[{()}]" :: [Char]))

recursiveBlock :: Parser a -> BraceType -> Parser (RecursiveBlock a)
recursiveBlock p t =
  try $ (,) <$> pure t <*> (char i *> extendableSyntaxForest p <* char o)
  where
    (i, o) = braceTypeChars t

plainTree :: Parser TextBuilder
plainTree =
  Renderer.extendableSyntaxTree (const mempty) <$>
  extendableSyntaxTree empty
