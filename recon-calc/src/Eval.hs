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
-- runEval inp = eval emptyEnv inp
--     where emptyEnv = Map.singleton "fix" 
runEval inp = do
    fix <- fixTerm
    let env = Map.singleton "fix" fix
    eval env inp

fixTerm :: ThrowsError Value
fixTerm = do
    parsed <- parseExp "\\f -> (\\x -> f (\\y -> x x y)) (\\x -> f (\\y -> x x y))"
    evaled <- eval Map.empty parsed
    return evaled

isVal env t = case t of
    (TmTrue _) -> True
    (TmFalse _) -> True
    (TmTag _ _ _ _) -> isVal env t
    (TmUnit _) -> True
    (TmInt _ _) -> True
    (TmAbs _ _ _ _) -> True
    _ -> False

-- evalTerm :: TermEnv -> Term -> ThrowsError Term
-- evalTerm env tm = case tm of
--     (TmIsZero _ n) -> do
--         (TmInt _ n') <- evalTerm env n
--         return $ if n' == 0 then (TmTrue DummyInfo) else (TmFalse DummyInfo)
--     (TmApp info fun arg) -> do
--         (TmClosure name body closure) <- evalTerm env fun
--         argVal <- evalTerm env arg
--         let env' = Map.insert name argVal closure
--         evalTerm env' body
--     (TmAbs info name _ body) -> return $ TmClosure name body env
--     (TmVar _ name) -> case Map.lookup name env of
--         Just v -> return v
--         Nothing -> error "can't find var"
--     (TmBinOp info op t1 t2) -> do
--         TmInt _ t1' <- evalTerm env t1
--         TmInt _ t2' <- evalTerm env t2
--         return $ getBinOp op t1' t2'
--     fixTerm@(TmFix info term) -> 
--         if isVal env term
--         then do
--             (TmAbs _ _ _ t2) -> 

eval :: TermEnv -> Term -> ThrowsError Value
eval env (TmTrue _) = return $ VBool True
eval env (TmFalse _) = return $ VBool False
eval env (TmInt _ n) = return $ VInt n
eval env (TmUnit _) = return $ VUnit
eval env (TmIf info cond true false) = do
    (VBool b) <- eval env cond
    case b of
        True -> eval env true
        False -> eval env false
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
eval env (TmLet info (name, t1) t2) = do
    v1 <- eval env t1
    let env' = Map.insert name v1 env
    eval env' t2
-- eval env (TmFix _ tm) = do
--     eval env (TmApp DummyInfo tm (TmFix DummyInfo tm))
-- eval env (TmLetRec info defs t2) = do
--     let (name, t1) = head defs
--     let newBody = TmFix DummyInfo (TmAbs DummyInfo name Nothing t1)
--     eval env (TmLet DummyInfo (name, newBody) t2)
eval env (TmApp info fun arg) = do
    funVal <- eval env fun
    case funVal of
        (VClosure name body closure) -> do
            argVal <- eval env arg
            let env' = Map.insert name argVal closure
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
getBinOp Minus a b = VInt $ a - b
