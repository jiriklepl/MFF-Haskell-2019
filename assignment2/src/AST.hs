module AST where

data Statement
    = EStmt Expression
    | CStmt ControlStatement
    | IStmt [Statement]
    deriving (Show)

data Expression
    = IdExpr Identifier
    | SLExpr StrLiteral
    | NLExpr NumLiteral
    | BOExpr Expression BinOp Expression
    | FCExpr FunctionCall
    deriving (Show)

data Identifier
    = Ident [Char]
    deriving (Show)

data StrLiteral
    = StrLit [Char]
    deriving (Show)

data BinOp
    = Plus
    | Minus
    | Mul
    | Div
    deriving (Show)

data FunctionDef
    = FunDef FunctionCall Statement
    deriving (Show)

data FunctionCall
    = FunCall Identifier [Identifier]
    deriving (Show)

data ControlStatement
    = IfStmt Expression Statement
    | IfElseStmt Expression Statement
    | ElseStmt Statement
    | WhileStmt Statement
    deriving (Show)
