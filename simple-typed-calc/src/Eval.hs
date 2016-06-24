module Eval (
    runEval
    , Value
) where

import Syntax
import Parser

import qualified Data.Map as Map
import Data.List

data Value
    = VInt Int
    | VBool Bool
    | VTag String Value
    | VUnit
    | VClosure String Term TermEnv

type TermEnv = Map.Map String Value

instance Show Value where
    show (VInt n) = show n
    show (VBool b) = show b
    show (VTag name t) = "Tag " ++ name ++ ", " ++ (show t)
    show (VClosure name body _) = "\\" ++ name ++ " -> " ++ show body

runEval inp = eval emptyEnv inp
    where emptyEnv = Map.empty

eval :: TermEnv -> Term -> Either String Value
eval env (TmTrue _) = Right $ VBool True
eval env (TmFalse _) = Right $ VBool False
eval env (TmInt _ n) = Right $ VInt n
eval env (TmUnit _) = Right $ VUnit
eval env (TmDataDec _ _ _) = Right $ VUnit
eval env (TmVar _ name) = case Map.lookup name env of
    Just v -> Right v
    Nothing -> Left $ "Can't find var: " ++ name ++ ", env: " ++ (show env)
eval env (TmAbs info name _ body) = return $ VClosure name body env
eval env (TmApp info t1 t2) = do
    (VClosure x body closure) <- eval env t1
    t2' <- eval env t2
    let env' = Map.insert x t2' closure
    eval env' body
eval env (TmBinOp info op t1 t2) = do
    VInt n1 <- eval env t1
    VInt n2 <- eval env t2
    return $ getBinOp op n1 n2
eval env (TmCase info tag branches) = do
    (VTag name v1) <- eval env tag
    let Just (x, t2) = lookup name branches
    let env' = Map.insert x v1 env
    eval env' t2
eval env (TmTag _ name t1 _) = do
       v1 <- eval env t1
       return $ VTag name v1
eval env err = error $ show err

getBinOp :: Op -> Int -> Int -> Value
getBinOp Times a b = VInt $ a * b
getBinOp Plus a b = VInt $ a + b
