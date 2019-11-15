module Main where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import Grammar
import Pretty

main :: IO ()
main = do
    contents <- getContents
    case parse program "" contents of
      Right a -> (putStrLn . ppshow) a
      Left e -> print e >> fail "parse error"
