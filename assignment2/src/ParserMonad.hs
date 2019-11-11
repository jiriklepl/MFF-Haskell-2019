module ParserMonad where

import Data.Map

data ParserMonad a = ParserMonad
  { indents :: [Int]
  }