module Lib
  ( matchPattern,
  )
where

import Data.Char
import Data.List

matchPattern :: String -> String -> Bool
matchPattern "\\d" = any isDigit
matchPattern "\\w" = any isAlphaNum
matchPattern pattern = any (isPrefixOf pattern) . tails
