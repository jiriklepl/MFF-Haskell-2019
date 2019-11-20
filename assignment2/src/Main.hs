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
import Analyzer

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
                putStrLn "code starts here:"
                (putStrLn . ppshow) (fst a)
                putStrLn "Compiler errors start here:"
                (putStrLn . ppshow . getReport) (snd a)
                putStrLn "\nAnalysis starts here:"
                (putStrLn . ppshow) (runAnalyzer (fst a))
            Left e -> print e >> fail "parse error"
    in do
        args <- getArgs
        sequence_ $ parseFile <$> if null args
            then ["-"]
            else args
