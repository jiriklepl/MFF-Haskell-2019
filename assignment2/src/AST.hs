module AST where

data Statement
    = EStmt Expression
    | DStmt FunctionDef
    | CStmt ControlStatement
    | IStmt [Statement]
    deriving (Show)

data Expression
    = IdExpr Identifier
    | SLExpr String
    | NLExpr Int
    | BOExpr Expression BinOp Expression
    | FCExpr FunctionCall
    deriving (Show)

newtype Identifier
    = Ident String
    deriving (Show)

newtype StrLiteral
    = StrLit String
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
