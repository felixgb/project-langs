module Eval where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map.Strict as Map

import Syntax

type Eval a = (StateT VarEnv (ThrowsError) a)

evalExpr :: Expr -> ThrowsError Expr
evalExpr expr = runEval emptyEnv (eval expr)
    where emptyEnv = Map.empty

runEval :: VarEnv -> Eval Expr -> ThrowsError Expr
runEval env m = evalStateT m env

lookupVar :: String -> Eval Expr
lookupVar x = do
    env <- get
    case Map.lookup x env of
        Nothing -> throwError $ ErrVarNotFound x
        Just val -> return val

insertIntoEnv :: String -> Expr -> Eval ()
insertIntoEnv name expr = do
    env <- get
    put $ Map.insert name expr env
    return ()

eval :: Expr -> Eval Expr
eval exp = case exp of
    (EVar info name) -> lookupVar name

    ex@(ELit info lit) -> return ex

    (ESeq first second) -> do
        eval first
        eval second

    (EAssign info name expr) -> do
        expr' <- eval expr
        insertIntoEnv name expr'
        return $ EUnit DummyInfo

    (EDef info name args body) -> do
        let scope = Map.empty
        insertIntoEnv name (EFunction name args body scope)
        return $ EUnit DummyInfo

    (EInvoke info name args) -> do
        evaledArgs <- mapM eval args
        func <- lookupVar name
        case func of
            (EFunction _ argNames body scope) -> do
                mapM_ eval $ zipWith (\name expr -> EAssign DummyInfo name expr) argNames args
                eval body

    (EBinexp info op left right) -> do
        left'  <- eval left
        right' <- eval right
        return $ lookupBinop op left' right'

lookupBinop :: Op -> Expr -> Expr -> Expr
lookupBinop Plus (ELit _ (LInt l)) (ELit _ (LInt r)) = ELit DummyInfo $ LInt (l + r)
