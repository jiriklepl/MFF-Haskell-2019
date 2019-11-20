module Analyzer(runAnalyzer) where

import AST
import AnalyzerState
import Control.Monad.State.Strict
import Data.Void

type Analyzer = State AnalyzerState

runAnalyzer :: Statement -> ErrorReport
runAnalyzer stmt =
    let
        state@AnalyzerState{errorReport = ErrorReport messages} = execState (analyze (Context []) stmt) initAnalyzer
    in ErrorReport $ reverse messages


class Analyze a where
    analyze :: Context -> a -> Analyzer ()

instance Analyze Statement where
    analyze context (Program stmts) = sequence_ $ analyze context <$> stmts

    analyze (Context stmts) defStmt@(DStmt (FunDef (FunCall (IdExpr ident) args) stmt)) = do
        state@AnalyzerState{idents = ids} <- get
        put (
            foldr
            define
            (enterScope $ define ident state)
            ((\(IdExpr id) -> id) <$> args))
        analyze (Context (defStmt : stmts)) stmt
        state <- get
        put (leaveScope state)

    analyze (Context stmts) ctrlStmt@(CStmt ctrlStmtBody) = analyze (Context $ ctrlStmt : stmts) ctrlStmtBody

    analyze context (IStmt stmts) = sequence_ $ analyze context <$> stmts

    analyze (Context stmts) exprStmt@(EStmt expr) = analyze (Context $ exprStmt : stmts) expr

instance Analyze ControlStatement where
    analyze context (IfStmt expr stmt) = analyze context expr >> analyze context stmt

    analyze context (ElifStmt expr stmt) = analyze context expr >> analyze context stmt

    analyze context (ElseStmt stmt) = analyze context stmt

    analyze context (WhileStmt expr stmt) = analyze context expr >> analyze context stmt

instance Analyze Expression where
    analyze context (IdExpr id) = do
        state <- get
        if isDefined id state
            then return ()
            else put $ makeNotDefined context id $ define id state

    analyze _ (SLExpr _) = return ()

    analyze _ (NLExpr _) = return ()

    analyze context (ParExpr expr) = analyze context expr

    analyze context (BOExpr (IdExpr id) (BinOp "=") expr2) = do
        analyze context expr2
        state <- get
        put $ define id state

    analyze context (BOExpr expr1 _ expr2) = do
        analyze context expr1
        analyze context expr2

    analyze context (FCExpr (FunCall expr exprs)) = do
        analyze context expr
        sequence_ $ analyze context <$> exprs
