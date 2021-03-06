module Grammar(program, getReport) where

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
import ErrorMessage

program :: Parsec Void String (Statement, ParserMonad)
program = runStateT ((many . try) (sc >> nl) >> sc >> (do
        (IStmt stmts) <- indentStatement
        many (sc >> nl)
        sc
        eof
        return $ Program stmts) <|> (eof >> return (Program []))) initParserMonad

getReport :: ParserMonad -> ErrorReport
getReport ParserMonad{errorReport = ErrorReport messages} =
    ErrorReport $ reverse messages

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
    CStmt . ElseStmt <$> statement

funDefinition :: Parser Statement
funDefinition = do
    rword "def"
    fdef <- funDefHeader
    void (symbol ":")
    let (FunCall (IdExpr ident) idents) = fdef
        in do
            state@ParserMonad{idents = ids} <- get
            put (
                foldr
                define
                (enterScope $ define ident state)
                ((\(IdExpr id) -> id) <$> idents))
    stmt <- statement
    state <- get
    put (leaveScope state)
    return $ DStmt $ FunDef fdef stmt

funDefHeader :: Parser FunctionCall
funDefHeader = do
    ident <- idExpressionPrimitive
    argsList <- parens (try defList <|> pure [])
    return $ FunCall ident argsList

defList :: Parser [Expression]
defList = do
    ident <- idExpressionPrimitive
    do
        void (symbol ",")
        args <- defList
        return (ident : args)
     <|> return [ident]

expression :: Parser Expression
expression = try binExpression
    <|> try assExpression
    <|> simpleExpression

simpleExpression :: Parser Expression
simpleExpression = try funExpression
    <|> simpleExpressionNoFun

simpleExpressionNoFun :: Parser Expression
simpleExpressionNoFun = parExpression
    <|> strExpression
    <|> numExpression
    <|> negExpression
    <|> idExpression

funExpression :: Parser Expression
funExpression = FCExpr <$> funCall

idExpression :: Parser Expression
idExpression = do
    idExpr@(IdExpr id) <- idExpressionPrimitive
    state <- get
    if isDefined id state
        then return idExpr
        else do
            put $ makeNotDefined id $ define id state
            return idExpr

idExpressionPrimitive :: Parser Expression
idExpressionPrimitive = IdExpr <$> identifier

strExpression :: Parser Expression
strExpression = SLExpr <$> quoted (many $ noneOf "\"")

numExpression :: Parser Expression
numExpression = NLExpr <$> integer

binExpression :: Parser Expression
binExpression = do
    expr1 <- simpleExpression
    sign <- binOperator
    BOExpr expr1 (BinOp sign) <$> expression

assExpression :: Parser Expression
assExpression = do
    idExpr@(IdExpr id) <- idExpressionPrimitive
    assSign <- symbol "="
    expr <- expression
    state <- get
    put $ define id state
    return $ BOExpr idExpr (BinOp assSign) expr

binOperator :: Parser String
binOperator =
    foldl
    (<|>)
    empty
    (symbol <$> ["+", "-", "*", "/", "<=", ">=", "<", ">"])

parExpression :: Parser Expression
parExpression = ParExpr <$> parens expression

negExpression :: Parser Expression
negExpression = NeExpr <$> (symbol "-" >> expression)

inlineStatement :: Parser Statement
inlineStatement = ifStatement
    <|> whileStatement
    <|> funDefinition
    <|> try elifStatement
    <|> elseStatement
    <|> exprStatement

commonNlIndentStmt :: Int -> Parser Statement
commonNlIndentStmt indent2 = do
    ParserMonad{indents = indent} <- get
    stmt <- inlineStatement
    state <- get
    put state{indents = indent2:indent}
    stmts <- many . try $ nlStatement
    state <- get
    put state{indents = indent}
    return $ IStmt (stmt:stmts)

nlStatement :: Parser Statement
nlStatement = do
    nl
    (many . try) (sc >> nl)
    indent2 <- indentifier
    ParserMonad{indents = indent} <- get
    case compare indent2 $ head indent of
        EQ -> inlineStatement
        GT -> commonNlIndentStmt indent2
        _  -> fail $ show indent2 ++ " vs " ++ show indent

statement :: Parser Statement
statement = (nl >> (many . try) (sc >> nl) >> indentStatement)
    <|> inlineStatement

exprStatement :: Parser Statement
exprStatement = EStmt <$> expression

indentStatement :: Parser Statement
indentStatement = do
    indent2 <- indentifier
    ParserMonad{indents = indent} <- get
    if indent2 > head indent
        then commonNlIndentStmt indent2
        else fail "indentation expected"

argumentList :: Parser [Expression]
argumentList = do
    ident <- expression
    (do
        void (symbol ",")
        args <- argumentList
        return (ident : args))
     <|> return [ident]

callList :: Parser [[Expression]]
callList = do
    args <- callArgs
    (do
        argsList <- callList
        return (args : argsList))
     <|> return [args]
    where callArgs = parens (try argumentList <|> pure [])

funCall :: Parser FunctionCall
funCall = do
    ident <- simpleExpressionNoFun
    composeCall ident <$> callList
    where
        composeCall ident [args] = FunCall ident args
        composeCall ident (args:argsSubList) =
            composeCall (FCExpr $ FunCall ident args) argsSubList
