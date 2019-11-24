module AST where

data Statement
    = Program [Statement]
    | DStmt FunctionDef
    | CStmt ControlStatement
    | IStmt [Statement]
    | EStmt Expression
    deriving (Show)

data Expression
    = IdExpr String
    | SLExpr String
    | NLExpr Integer
    | NeExpr Expression
    | ParExpr Expression
    | BOExpr Expression BinOp Expression
    | FCExpr FunctionCall
    deriving (Show)

newtype BinOp
    = BinOp String
    deriving (Show)

data FunctionDef
    = FunDef FunctionCall Statement
    deriving (Show)

data FunctionCall
    = FunCall Expression [Expression]
    deriving (Show)

data ControlStatement
    = IfStmt Expression Statement
    | ElifStmt Expression Statement
    | ElseStmt Statement
    | WhileStmt Expression Statement
    deriving (Show)
