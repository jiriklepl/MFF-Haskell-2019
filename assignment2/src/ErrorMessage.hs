module ErrorMessage where

import AST

data ErrorReport = ErrorReport [ErrorMessage] deriving(Show)
data Context = Context [Statement] deriving(Show)

data ErrorMessage
    = NotDefinedContextMessage Context String
    | NotDefinedMessage String Integer
    deriving (Show)
