module Main where

import BasePrelude
import Record.Syntax
import Conversion
import Conversion.Text
import qualified Data.Text.IO as Text

main =
  either (putStrLn . showString "Error: " . show) (Text.putStrLn . convert) . processModule =<<
  Text.readFile "samples/1.hs"


