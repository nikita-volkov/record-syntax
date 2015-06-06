module Record.Syntax.Shared where

import Record.Syntax.Prelude


marker :: IsString a => a
marker =
  "Ѣ"


type Error =
  (Position, String)
  

type Position =
  (Int, Int)


data Level =
  Level_Type |
  Level_Exp |
  Level_Pat |
  Level_Decl
  deriving (Show)


data BraceType =
  BraceType_Curly | 
  BraceType_Round | 
  BraceType_Square
  deriving (Generic, Show, Eq, Ord, Enum, Bounded)

braceTypeChars :: BraceType -> (Char, Char)
braceTypeChars =
  \case
    BraceType_Curly -> ('{', '}')
    BraceType_Round -> ('(', ')')
    BraceType_Square -> ('[', ']')
    

data ExtendableSyntaxTree a =
  ExtendableSyntaxTree_Extension a |
  ExtendableSyntaxTree_RecursiveBlock (RecursiveBlock a) |
  ExtendableSyntaxTree_Lexeme TextBuilder
  deriving (Show, Eq, Functor, Foldable, Traversable)


newtype ExtendableSyntaxForest a =
  ExtendableSyntaxForest ([ExtendableSyntaxTree a])
  deriving (Show, Eq, Functor, Foldable, Traversable)


type RecursiveBlock a =
  (BraceType, ExtendableSyntaxForest a)


data ExtensionExp a =
  ExtensionExp_Record (RecordExp a) |
  ExtensionExp_Label Label
  deriving (Show, Eq, Functor, Foldable, Traversable)


newtype RecordExp a =
  RecordExp (Bool, [(TextBuilder, Maybe a)])
  deriving (Show, Eq, Functor, Foldable, Traversable)


type Label =
  TextBuilder


data ExtensionType =
  ExtensionType_Record RecordType


type RecordType =
  (Bool, [(TextBuilder, ExtendableSyntaxForest ExtensionType)])