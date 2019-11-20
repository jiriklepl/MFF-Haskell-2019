module Main where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Environment
import System.IO

import Grammar
import Pretty

main :: IO ()
main = let
    parseFile arg = do
        contents <- if arg == "-"
            then getContents
            else do
                handle <- openFile arg ReadMode
                hGetContents handle
        case parse program arg contents of
            Right a -> do
                (putStrLn . ppshow . getReport) (snd a)
                putStrLn "code starts here:"
                (putStrLn . ppshow) (fst a)
            Left e -> print e >> fail "parse error"
    in do
        args <- getArgs
        sequence_ $ parseFile <$> if null args
            then ["-"]
            else args
