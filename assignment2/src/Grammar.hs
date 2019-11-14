module Grammar where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST
import Parser

program = between sc eof (statement 0)

ccStatement :: Int -> Parser (Expression, Statement)
ccStatement indent = do
    cond <- expression
    void (symbol ":")
    nl
    stmt <- statement indent
    return (cond, stmt)

ifStatement :: Int -> Parser Statement
ifStatement indent = do
    rword "if"
    (expr, stmt) <- ccStatement indent
    return $ CStmt (IfStmt expr stmt)

whileStatement :: Int -> Parser Statement
whileStatement indent = do
    rword "while"
    (expr, stmt) <- ccStatement indent
    return $ CStmt (WhileStmt expr stmt)

elseStatement :: Int -> Parser Statement
elseStatement indent = do
    rword "else"
    void (symbol ":")
    nl
    stmt <- statement indent
    return $ CStmt (ElseStmt stmt)

funDefinition :: Int -> Parser Statement
funDefinition indent = do
    rword "def"
    fcall <- funCall
    void (symbol ":")
    nl
    stmt <- statement indent
    return $ DStmt (FunDef fcall stmt)

expression :: Parser Expression
expression = funExpression
    <|> binExpression
    <|> strExpression
    <|> numExpression
    <|> idExpression

funExpression :: Parser Expression
funExpression = FCExpr <$> funCall

idExpression :: Parser Expression
idExpression = IdExpr <$> identifier

strExpression :: Parser Expression
strExpression = SLExpr <$> (quoted $ (many $ noneOf "\""))

numExpression :: Parser Expression
numExpression = NLExpr <$> integer

binExpression :: Parser Expression
binExpression = do
    expr1 <- expression
    sc
    symbol <- oneOf "+-*/"
    expr2 <- expression
    return $ case symbol of
        '+' -> BOExpr expr1 Plus expr2
        '-' -> BOExpr expr1 Minus expr2
        '*' -> BOExpr expr1 Mul expr2
        '/' -> BOExpr expr1 Div expr2

statement :: Int -> Parser Statement
statement indent = ifStatement indent
        <|> whileStatement indent
        <|> elseStatement indent
        <|> funDefinition indent



argumentList :: String -> Parser [String]
argumentList ident = (do
        void (symbol ")")
        return [ident])
    <|> (do
        void (symbol ",")
        ident1 <- identifier
        args <- argumentList ident1
        return (ident : args))

funCall :: Parser FunctionCall
funCall = do
    ident <- identifier
    void (symbol "(")
    ident1 <- identifier
    args <- argumentList ident1
    return $ FunCall ident args
