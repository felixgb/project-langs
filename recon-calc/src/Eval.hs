module Eval (
    runEval
    , Value
) where

import Syntax
import Parser

import Control.Monad.Except

import qualified Data.Map as Map
import Data.List

data Value
    = VInt Int
    | VBool Bool
    | VTag String Value
    | VPair [Value]
    | VUnit
    | VFold Value
    | VUnfold Value
    | VClosure String Term TermEnv

type TermEnv = Map.Map String Value

instance Show Value where
    show (VInt n) = show n
    show (VBool b) = if b then "true" else "false"
    show (VTag name t) = "Tag " ++ name ++ ", " ++ (show t)
    show (VPair [v1, v2]) = "(" ++ (show v1) ++ ", " ++ (show v2) ++ ")"
    show (VClosure name body _) = "\\" ++ name ++ " -> " ++ show body
    show (VFold v) = "fold: " ++ (show v)
    show (VUnfold v) = "unfold" ++ (show v)
    show VUnit = "unit"

runEval :: Term -> ThrowsError Value
runEval inp = eval emptyEnv inp
    where emptyEnv = Map.empty

eval :: TermEnv -> Term -> ThrowsError Value
eval env (TmTrue _) = return $ VBool True
eval env (TmFalse _) = return $ VBool False
eval env (TmInt _ n) = return $ VInt n
eval env (TmUnit _) = return $ VUnit
eval env (TmPair _ tms) = do
    vs <- mapM (eval env) tms
    return $ VPair vs
eval env (TmProj _ t idx) = do
    (VPair vs) <- eval env t
    return $ vs !! idx
eval env (TmDataDec _ _ _) = return $ VUnit
eval env (TmVar _ name) = case Map.lookup name env of
    Just v -> return v
    Nothing -> throwError $ ErrDefault $ "Can't find var: " ++ name ++ ", env: " ++ (show env)
eval env (TmAbs info name _ body) = return $ VClosure name body env
eval env (TmApp info t1 t2) = do
    t1' <- eval env t1
    case t1' of
        (VClosure x body closure) -> do
            t2' <- eval env t2
            let env' = Map.insert x t2' closure
            eval env' body
eval env (TmBinOp info op t1 t2) = do
    VInt n1 <- eval env t1
    VInt n2 <- eval env t2
    return $ getBinOp op n1 n2
eval env (TmIsZero info t) = do
    VInt v <- eval env t
    return $ VBool $ if v == 0 then True else False
eval env (TmCase info tag branches) = do
    (VTag name v1) <- eval env tag
    let Just (x, t2) = lookup name branches
    let env' = Map.insert x v1 env
    eval env' t2
eval env (TmTag _ name t1 _) = do
       v1 <- eval env t1
       return $ VTag name v1
eval env (TmUnfold _ _ (TmFold _ _ t)) = eval env t
eval env (TmFold _ _ t) = eval env t
eval env (TmUnfold _ _ t) = eval env t
eval env err = error $ show err

getBinOp :: Op -> Int -> Int -> Value
getBinOp Times a b = VInt $ a * b
getBinOp Plus a b = VInt $ a + b
