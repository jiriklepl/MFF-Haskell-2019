module Grammar where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST
import Parser

program = do
    (IStmt stmts) <- indentStatement [-1]
    eof
    return $ Program stmts

ccStatement :: [Int] -> Parser (Expression, Statement)
ccStatement indent = do
    cond <- expression
    void (symbol ":")
    stmt <- statement indent
    return (cond, stmt)

ifStatement :: [Int] -> Parser Statement
ifStatement indent = do
    rword "if"
    (expr, stmt) <- ccStatement indent
    return $ CStmt (IfStmt expr stmt)

elifStatement :: [Int] -> Parser Statement
elifStatement indent = do
    rword "else"
    rword "if"
    (expr, stmt) <- ccStatement indent
    return $ CStmt (ElifStmt expr stmt)

whileStatement :: [Int] -> Parser Statement
whileStatement indent = do
    rword "while"
    (expr, stmt) <- ccStatement indent
    return $ CStmt (WhileStmt expr stmt)

elseStatement :: [Int] -> Parser Statement
elseStatement indent = do
    rword "else"
    void (symbol ":")
    stmt <- statement indent
    return $ CStmt (ElseStmt stmt)

funDefinition :: [Int] -> Parser Statement
funDefinition indent = do
    rword "def"
    fcall <- funCall
    void (symbol ":")
    stmt <- statement indent
    return $ DStmt (FunDef fcall stmt)

expression :: Parser Expression
expression = try binExpression
    <|> simpleExpression

simpleExpression :: Parser Expression
simpleExpression = parExpression
    <|> strExpression
    <|> numExpression
    <|> try funExpression
    <|> idExpression

funExpression :: Parser Expression
funExpression = FCExpr <$> funCall

idExpression :: Parser Expression
idExpression = IdExpr <$> identifier

strExpression :: Parser Expression
strExpression = SLExpr <$> quoted  (many $ noneOf "\"")

numExpression :: Parser Expression
numExpression = NLExpr <$> integer

binExpression :: Parser Expression
binExpression = do
    expr1 <- simpleExpression
    symbol <- binOperator
    expr2 <- expression
    return $ BOExpr expr1 (BinOp symbol) expr2

binOperator :: Parser String
binOperator = foldl (<|>) empty (symbol <$> ["+", "-", "*", "/", "=", "<=", ">=", "<", ">"])

parExpression :: Parser Expression
parExpression = parens expression

inlineStatement :: [Int] -> Parser Statement
inlineStatement indent = ifStatement indent
    <|> whileStatement indent
    <|> funDefinition indent
    <|> try (elifStatement indent)
    <|> elseStatement indent
    <|> exprStatement

nlStatement :: [Int] -> Parser Statement
nlStatement indent = do
    nl
    (many . try) (sc >> nl)
    indent2 <- indentifier
    if indent2 == head indent
        then inlineStatement indent
        else fail  $ show indent2 ++ " vs " ++ show indent

statement :: [Int] -> Parser Statement
statement indent = (nl >> (many . try) (sc >> nl) >> indentStatement indent)
    <|> inlineStatement indent

exprStatement :: Parser Statement
exprStatement = EStmt <$> expression

indentStatement :: [Int] -> Parser Statement
indentStatement indent = do
    indent2 <- indentifier
    case compare indent2 $ head indent of
        GT -> do
            stmt <- inlineStatement (indent2:indent)
            stmts <- many . try $ nlStatement (indent2:indent)
            return $ IStmt (stmt:stmts)
        _ -> fail "indentation expected"

argumentList :: Parser [Expression]
argumentList =
    try (do
        ident <- expression
        void (symbol ",")
        args <- argumentList
        return (ident : args))
    <|> (do
        ident <- expression
        return [ident])

funCall :: Parser FunctionCall
funCall = do
    ident <- idExpression
    args <- parens (try argumentList <|> pure [])
    return $ FunCall ident args
