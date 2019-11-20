module Pretty where

import Text.PrettyPrint

import AST

class Pretty a where
    pretty :: a -> Doc

tab :: Doc -> Doc
tab = nest 4

ppshow :: Pretty a => a -> String
ppshow = renderStyle (style {mode=PageMode, lineLength = 80}) . pretty

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
