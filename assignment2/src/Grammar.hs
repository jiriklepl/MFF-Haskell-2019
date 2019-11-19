module Grammar where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Control.Monad.State.Strict
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST
import Parser
import ParserMonad

program :: Parsec Void String Statement
program = evalStateT (do
    (IStmt stmts) <- sc >> indentStatement
    many (sc >> nl)
    sc
    eof
    return $ Program stmts) ParserMonad{indents = [-1], idents=[], errorReport = []}

ccStatement :: Parser (Expression, Statement)
ccStatement = do
    cond <- expression 
    void (symbol ":")
    stmt <- statement
    return (cond, stmt)

ifStatement :: Parser Statement
ifStatement = do
    rword "if"
    (expr, stmt) <- ccStatement
    return $ CStmt (IfStmt expr stmt)

elifStatement :: Parser Statement
elifStatement = do
    rword "else"
    rword "if"
    (expr, stmt) <- ccStatement
    return $ CStmt (ElifStmt expr stmt)

whileStatement :: Parser Statement
whileStatement = do
    rword "while"
    (expr, stmt) <- ccStatement
    return $ CStmt (WhileStmt expr stmt)

elseStatement :: Parser Statement
elseStatement = do
    rword "else"
    void (symbol ":")
    stmt <- statement
    return $ CStmt (ElseStmt stmt)

funDefinition :: Parser Statement
funDefinition = do
    rword "def"
    fcall <- funCall
    void (symbol ":")
    stmt <- statement
    return $ DStmt (FunDef fcall stmt)

expression :: Parser Expression
expression = try binExpression
    <|> simpleExpression

simpleExpression :: Parser Expression
simpleExpression = try funExpression
    <|> strExpression
    <|> numExpression
    <|> parExpression
    <|> idExpression

simpleExpressionNoFun :: Parser Expression
simpleExpressionNoFun = parExpression
    <|> strExpression
    <|> numExpression
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
parExpression = ParExpr <$> parens expression

inlineStatement :: Parser Statement
inlineStatement = ifStatement
    <|> whileStatement
    <|> funDefinition
    <|> try elifStatement
    <|> elseStatement
    <|> exprStatement

nlStatement :: Parser Statement
nlStatement = do
    nl
    (many . try) (sc >> nl)
    indent2 <- indentifier
    ParserMonad{indents = indent} <- get
    if indent2 == head indent then
        inlineStatement
    else if indent2 > head indent then do
        stmt <- inlineStatement
        state <- get
        put state{indents = indent2:indent}
        stmts <- many . try $ nlStatement
        return $ IStmt (stmt:stmts)
    else fail  $ show indent2 ++ " vs " ++ show indent

statement :: Parser Statement
statement = (nl >> (many . try) (sc >> nl) >> indentStatement)
    <|> inlineStatement

exprStatement :: Parser Statement
exprStatement = EStmt <$> expression

indentStatement :: Parser Statement
indentStatement = do
    indent2 <- indentifier
    ParserMonad{indents = indent} <- get
    if indent2 > head indent then do
        stmt <- inlineStatement
        state <- get
        put state{indents = indent2:indent}
        stmts <- many . try $ nlStatement
        return $ IStmt (stmt:stmts)
    else fail "indentation expected"

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

callList :: Parser [[Expression]]
callList =
    try (do
        args <- callArgs
        argsList <- callList
        return (args : argsList))
    <|> (do
        args <- callArgs
        return [args])
    where callArgs = parens (try argumentList <|> pure [])


funCall :: Parser FunctionCall
funCall = do
    ident <- try simpleExpressionNoFun
    argsList <- callList
    return $ composeCall ident argsList
    where
        composeCall ident [args] = FunCall ident args
        composeCall ident (args:argsSubList) =
            composeCall (FCExpr $ FunCall ident args) argsSubList
