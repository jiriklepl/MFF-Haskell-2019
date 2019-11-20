module ErrorMessage where

data ErrorReport = ErrorReport [ErrorMessage]

data ErrorMessage
    = SimpleErrorMessage String Integer
    | NotDefinedMessage String Integer
    deriving (Show)
