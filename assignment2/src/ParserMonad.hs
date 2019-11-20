module ParserMonad where

import ErrorMessage
import Standard

enterScope parserM@ParserMonad{idents = ids} = parserM{idents = []:ids}
leaveScope parserM@ParserMonad{idents = ids} = parserM{idents = tail ids}

isDefined :: String -> ParserMonad -> Bool
isDefined id parserM@ParserMonad{idents = ids} =
    let isElem _ [] = False
        isElem id (some : rest) = elem id some || isElem id rest
    in isElem id ids

define :: String -> ParserMonad -> ParserMonad
define id parserM@ParserMonad{idents = ids} =
    let defineHelp _ [] = parserM{idents = [[id]]}
        defineHelp id (some:rest) = parserM{idents = (id:some) : rest}
    in defineHelp id ids

makeNotDefined :: String -> ParserMonad -> ParserMonad
makeNotDefined message parserM@ParserMonad{
    errorReport = (ErrorReport messages),
    lineNo = ln} = parserM{
        errorReport = ErrorReport (NotDefinedMessage message ln : messages)}

data ParserMonad = ParserMonad
    { indents :: [Int]
    , idents :: [[String]]
    , errorReport :: ErrorReport
    , lineNo :: Integer
    }

initParserMonad = ParserMonad
    { indents = [-1]
    , idents = [standardIdentifiers]
    , errorReport = ErrorReport []
    , lineNo = 1
    }
