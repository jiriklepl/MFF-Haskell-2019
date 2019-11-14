module Main where

import Grammar
import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = do
    contents <- getContents
    case parse program "" contents of
      Right a -> print a
      Left e -> print e >> fail "parse error"
