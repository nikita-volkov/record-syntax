module Record.Syntax.LevelReifier
(
  reify,
)
where

import Record.Syntax.Prelude
import Record.Syntax.Shared
import qualified Language.Haskell.Exts as E
import qualified Record.Syntax.LevelReifier.Levels as Levels


-- |
-- Parses the code using "haskell-src-exts", reifying the AST levels.
reify :: Level -> String -> Either Error [Level]
reify =
  fmap convertResult . \case
    Level_Decl   -> fmap Levels.module_ . E.parseModuleWithMode parseMode
    Level_Type   -> fmap Levels.type_ . E.parseTypeWithMode parseMode
    Level_Exp    -> fmap Levels.exp . E.parseExpWithMode parseMode
    Level_Pat    -> fmap Levels.pat . E.parsePatWithMode parseMode

parseMode :: E.ParseMode
parseMode =
  E.defaultParseMode {
    E.extensions = map (E.EnableExtension) (enumFrom minBound)
  }

convertResult :: E.ParseResult a -> Either Error a
convertResult =
  \case
    E.ParseOk a -> Right a
    E.ParseFailed l m -> Left (convertSrcLoc l, m)

convertSrcLoc :: E.SrcLoc -> Position
convertSrcLoc (E.SrcLoc _ l c) =
  (,) (fromIntegral l) (fromIntegral c)
