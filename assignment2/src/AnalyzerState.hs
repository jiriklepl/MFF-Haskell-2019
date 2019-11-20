module AnalyzerState
    ( module AnalyzerState
    , Context(..)
    , ErrorReport(..)
    ) where

import AST
import ErrorMessage
import Standard


enterScope analyzer@AnalyzerState{idents = ids} = analyzer{idents = []:ids}
leaveScope analyzer@AnalyzerState{idents = ids} = analyzer{idents = tail ids}

isDefined :: String -> AnalyzerState -> Bool
isDefined id analyzer@AnalyzerState{idents = ids} =
    let isElem _ [] = False
        isElem id (some : rest) = elem id some || isElem id rest
    in isElem id ids

define :: String -> AnalyzerState -> AnalyzerState
define id analyzer@AnalyzerState{idents = ids} =
    let defineHelp _ [] = analyzer{idents = [[id]]}
        defineHelp id (some:rest) = analyzer{idents = (id:some) : rest}
    in defineHelp id ids

makeNotDefined :: Context -> String -> AnalyzerState -> AnalyzerState
makeNotDefined context id analyzer@AnalyzerState{
    errorReport = (ErrorReport messages)} = analyzer{
        errorReport = ErrorReport (NotDefinedContextMessage context id : messages)}

data AnalyzerState = AnalyzerState
    { errorReport :: ErrorReport
    , idents :: [[String]]
    }

initAnalyzer = AnalyzerState
    { idents = [standardIdentifiers]
    , errorReport = ErrorReport []
    }
