module AST where

data Statement
    = EStmt Expression
    | DStmt FunctionDef
    | CStmt ControlStatement
    | IStmt [Statement]
    deriving (Show)

data Expression
    = IdExpr String
    | SLExpr String
    | NLExpr Integer
    | BOExpr Expression BinOp Expression
    | FCExpr FunctionCall
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
    = FunCall String [String]
    deriving (Show)

data ControlStatement
    = IfStmt Expression Statement
    | IfElseStmt Expression Statement
    | ElseStmt Statement
    | WhileStmt Expression Statement
    deriving (Show)
