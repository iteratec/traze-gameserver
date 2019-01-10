module Traze.Internal.Utils where

import Data.Char

cut4LowerCase :: String -> String
cut4LowerCase input = ((toLower . head . drop 4) input) : (drop 5 input)
