module Record.Syntax.Renderer where

import Record.Syntax.Prelude
import Record.Syntax.Shared
import qualified Data.Text.Lazy.Builder as TLB


type Renderer a =
  a -> TextBuilder

extendableSyntaxForest :: Renderer a -> Renderer (ExtendableSyntaxForest a)
extendableSyntaxForest sub (ExtendableSyntaxForest list) =
  foldMap (extendableSyntaxTree sub) list

extendableSyntaxTree :: Renderer a -> Renderer (ExtendableSyntaxTree a)
extendableSyntaxTree sub =
  \case
    ExtendableSyntaxTree_Extension x -> sub x
    ExtendableSyntaxTree_RecursiveBlock x -> recursiveBlock sub x
    ExtendableSyntaxTree_Lexeme x -> x

recursiveBlock :: Renderer a -> Renderer (RecursiveBlock a)
recursiveBlock sub (bt, es) =
  convert i <> extendableSyntaxForest sub es <> convert o
  where
    (i, o) = braceTypeChars bt

recordExp :: Renderer a -> Renderer (RecordExp a)
recordExp sub (RecordExp (strict, sections)) =
  flip evalState 0 $ do
    sectionStrings <-
      forM sortedSections $ \(name, value) -> case value of
        Nothing -> do
          modify succ
          var <- fmap varName get
          return $ fieldNameExp name <> " " <> var
        Just a -> return $ fieldNameExp name <> " (" <> sub a <> ")"
    numArgs <- get
    let exp = 
          "(" <>
          bool "Record.lazyRecord" "Record.strictRecord" strict <> 
          convert (show (length sections)) <>
          " " <> mconcat (intersperse " " sectionStrings) <>
          ")"
    case numArgs of
      0 -> return $ exp
      n -> return $ "\\" <> mconcat (intersperse " " (map varName [1 .. numArgs])) <> " -> " <> exp
  where
    varName n = 
      "ѣ" <> convert (show n)
    sortedSections =
      sortWith fst sections

labelExp :: Renderer Label
labelExp x =
  "(Record.fieldLens " <> fieldNameExp x <> ")"

fieldNameExp :: Renderer TextBuilder
fieldNameExp x =
  "(undefined :: Record.FieldName \"" <> x <> "\")"

recordType :: Renderer RecordType
recordType (strict, sections) =
  "(" <> recordName <> " " <> renderedSections <> ")"
  where
    recordName =
      basis <> arity
      where
        basis =
          if strict
            then "Record.StrictRecord"
            else "Record.LazyRecord"
        arity =
          convert $ show $ length sections
    renderedSections =
      mconcat (intersperse " " (map section (sortWith fst sections)))
      where
        section (name, forest) =
          "\"" <> name <> "\"" <> " " <> "(" <> extendableSyntaxForest extensionType forest <> ")"

extensionType :: Renderer ExtensionType
extensionType =
  \case
    ExtensionType_Record x -> recordType x

