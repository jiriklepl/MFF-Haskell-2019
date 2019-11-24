module Pretty(ppshow) where

import Text.PrettyPrint

import AST
import ErrorMessage

class Pretty a where
    pretty :: a -> Doc

tab :: Doc -> Doc
tab = nest 4

ppshow :: Pretty a => a -> String
ppshow = renderStyle (style {mode=PageMode, lineLength = 80}) . pretty

-- AST:
instance Pretty FunctionCall where
    pretty (FunCall expr exprs) =
        pretty expr <> (parens . sep . punctuate comma) (pretty <$> exprs)

instance Pretty Statement where
    pretty (Program stmts) = vcat (pretty <$> stmts)

    pretty (DStmt (FunDef funCall (IStmt stmts))) =
        text "def"
        <+> pretty funCall
        <> char ':'
        <+> lbrace
        $+$ tab (foldr ($+$) empty (pretty <$> stmts))
        $+$ rbrace
        $+$ text ""

    pretty (DStmt (FunDef funCall stmt)) =
        text "def"
        <+> pretty funCall
        <> char ':'
        <+> lbrace
        $+$ tab (pretty stmt)
        $+$ rbrace
        $+$ text ""

    pretty (CStmt cstmt) = pretty cstmt

    pretty (IStmt stmts) =
        lbrace
        $+$ tab (foldr ($+$) empty (pretty <$> stmts))
        $+$ rbrace
    pretty (EStmt expr) = pretty expr <> char ';'

instance Pretty Expression where
    pretty (IdExpr id) = text id

    pretty (SLExpr str) = char '"' <> text str <> char '"'

    pretty (ParExpr expr) = parens $ pretty expr

    pretty (NLExpr num) = integer num

    pretty (BOExpr expr1 (BinOp str) expr2) =
        pretty expr1
        <+> text str
        <+> pretty expr2

    pretty (NeExpr expr) = char '-' <> pretty expr

    pretty (FCExpr funCall) = pretty funCall

instance Pretty ControlStatement where
    pretty (IfStmt expr (IStmt stmts)) =
        text "if"
        <+> pretty expr
        <> char ':'
        <+> lbrace
        $+$ tab (foldr ($+$) empty (pretty <$> stmts))
        $+$ rbrace
        $+$ text ""

    pretty (IfStmt expr stmt) =
        text "if"
        <+> pretty expr
        <> char ':'
        <+> lbrace
        $+$ tab (pretty stmt)
        $+$ rbrace
        $+$ text ""

    pretty (ElifStmt expr (IStmt stmts)) =
        text "else"
        <+> text "if"
        <+> pretty expr
        <> char ':'
        <+> lbrace
        $+$ tab (foldr ($+$) empty (pretty <$> stmts))
        $+$ rbrace
        $+$ text ""

    pretty (ElifStmt expr stmt) =
        text "else"
        <+> text "if"
        <+> pretty expr
        <> char ':'
        <+> lbrace
        $+$ tab (pretty stmt)
        $+$ rbrace
        $+$ text ""

    pretty (ElseStmt (IStmt stmts)) =
        text "else"
        <> char ':'
        <+> lbrace
        $+$ tab (foldr ($+$) empty (pretty <$> stmts))
        $+$ rbrace
        $+$ text ""

    pretty (ElseStmt stmt) =
        text "else"
        <> char ':'
        <+> lbrace
        $+$ tab (pretty stmt)
        $+$ rbrace
        $+$ text ""

    pretty (WhileStmt expr (IStmt stmts)) =
        text "while"
        <+> pretty expr
        <> char ':'
        <+> lbrace
        $+$ tab (foldr ($+$) empty (pretty <$> stmts))
        $+$ rbrace
        $+$ text ""

    pretty (WhileStmt expr stmt) =
        text "while"
        <+> pretty expr
        <> char ':'
        <+> lbrace
        $+$ tab (pretty stmt)
        $+$ rbrace
        $+$ text ""

-- Errors:
instance Pretty ErrorReport where
    pretty (ErrorReport []) = text "No errors found."

    pretty (ErrorReport messages) =
        int len
        <+> text (if len == 1 then "error" else "errors")
        <+> text "found:"
        $+$ vcat (pretty <$> messages)
        where len = length messages

instance Pretty ErrorMessage where
    pretty (NotDefinedMessage id ln) =
        text "Error: Undeclared identifier '"
        <> text id
        <> text "' at line:"
        <+> integer ln

    pretty (NotDefinedContextMessage context id) =
        text "Error: Undeclared identifier '"
        <> text id
        <> char '\''
        $+$ pretty context

-- Analyzer
instance Pretty Context where
    pretty (Context []) = text ""

    pretty (Context (DStmt (FunDef fcall _) : stmts)) =
        text " .. found in a function definition '"
        <> pretty fcall
        <> char '\''
        $+$ pretty (Context stmts)

    pretty (Context (EStmt expr : stmts)) =
        text " .. found in an expression '"
        <> pretty expr
        <> char '\''
        $+$ pretty (Context stmts)
