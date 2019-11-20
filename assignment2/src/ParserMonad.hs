module ParserMonad where

import ErrorMessage

enterScope parserM@ParserMonad{idents = ids} = parserM{idents = []:ids}
leaveScope parserM@ParserMonad{idents = ids} = parserM{idents = tail ids}

isDefined id parserM@ParserMonad{idents = ids} =
    let isElem _ [] = False
        isElem id (some:rest) = elem id some || isElem id rest
    in isElem id ids

data ParserMonad = ParserMonad
    { indents :: [Int]
    , idents :: [[String]]
    , errorReport :: [ErrorMessage]
    }

initParserMonad = ParserMonad{indents = [-1], idents=[[]], errorReport = []}
