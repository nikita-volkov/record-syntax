module Record.Syntax where

import Record.Syntax.Prelude
import Record.Syntax.Shared
import qualified Record.Syntax.LevelReifier as LevelReifier
import qualified Record.Syntax.Position as Position
import qualified Record.Syntax.Parser as Parser
import qualified Record.Syntax.Renderer as Renderer


processModule :: Text -> Either Error TextBuilder
processModule input =
  join $ runParser pass1Parser
  where
    runParser p =
      Parser.run p input
    pass1Parser =
      liftA2 mappend <$> fmap pure head <*> body
      where
        head =
          flip fmap Parser.moduleHead $ \(output, t) -> output <> "import qualified Record" <> ending t
          where
            ending = 
              \case
                Parser.LineType_Space -> ""
                Parser.LineType_Comment -> " "
                Parser.LineType_Import -> "; "
                Parser.LineType_Other -> "\n"
        body =
          do
            offset <- Parser.position
            forest <- Parser.total $ Parser.extendableSyntaxForest Parser.unparsedExtensionLexeme
            return $ processExtensionForest Level_Decl forest

process :: Level -> Text -> Either Error TextBuilder
process level input =
  do
    forest <- Parser.run parser $ input
    processExtensionForest level forest
  where
    parser =
      Parser.extendableSyntaxForest Parser.unparsedExtensionLexeme

processExtensionForest :: Level -> ExtendableSyntaxForest TextBuilder -> Either Error TextBuilder
processExtensionForest level forest =
  do
    levels <- reifyLevels level forest
    forest' <- zipTraversableWithM (\a b -> processExtension b (convert a)) forest levels
    return $ Renderer.extendableSyntaxForest id forest'
  where
    reifyLevels level =
      LevelReifier.reify level . convert .
      Renderer.extendableSyntaxForest (const marker)

processExtension :: Level -> Text -> Either Error TextBuilder
processExtension level input =
  join $ Parser.run (parser level) input
  where
    parser =
      \case
        Level_Exp -> fmap processExtensionExp Parser.extensionExp
        Level_Type -> fmap processExtensionType Parser.extensionType

processExtensionExp :: ExtensionExp (Position, TextBuilder) -> Either Error TextBuilder
processExtensionExp =
  \case
    ExtensionExp_Record x -> processRecordExp x
    ExtensionExp_Label x -> pure $ Renderer.labelExp x

processRecordExp :: RecordExp (Position, TextBuilder) -> Either Error TextBuilder
processRecordExp =
  fmap (Renderer.recordExp id) .
  traverse (\(p, i) -> offsetResult p $ process Level_Exp $ convert i)

processExtensionType :: ExtensionType -> Either Error TextBuilder
processExtensionType =
  \case
    ExtensionType_Record x -> pure $ Renderer.recordType x


offsetResult :: Position -> Either Error a -> Either Error a
offsetResult p =
  either (Left . (\f (a, b) -> (f a, b)) (Position.add p)) Right

