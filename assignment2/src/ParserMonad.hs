module ParserMonad where

import ErrorMessage

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

writeError :: ErrorMessage -> ParserMonad -> ParserMonad
writeError message parserM@ParserMonad{errorReport = report} =
    parserM{errorReport = message:report}

data ParserMonad = ParserMonad
    { indents :: [Int]
    , idents :: [[String]]
    , errorReport :: [ErrorMessage]
    }

initParserMonad = ParserMonad{indents = [-1], idents = [], errorReport = []}
