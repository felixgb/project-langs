module Eval where

import Syntax
import Parser

import qualified Data.Map as Map

data Value
    = VInt Int
    | VBool Bool
    | VClosure String Term TermEnv

type TermEnv = Map.Map String Value

instance Show Value where
    show (VInt n) = show n
    show (VBool b) = show b
    show (VClosure _ _ _) = "<<closure>>"

eval :: TermEnv -> Term -> Value
eval env (TmTrue _) = VBool True
eval env (TmFalse _) = VBool False
eval env (TmInt _ n) = VInt n
eval env (TmVar _ name) = let Just v = Map.lookup name env in v
eval env (TmAbs info var _ body) = VClosure var body env
eval env (TmApp info t1 t2) = eval env' body
    where (VClosure x body closure) = eval env t1
          t2' = eval env t2
          env' = Map.insert x t2' closure
eval env (TmBinOp info op t1 t2) = getBinOp op n1 n2
    where VInt n1 = eval env t1
          VInt n2 = eval env t2

getBinOp :: Op -> Int -> Int -> Value
getBinOp Times a b = VInt $ a * b
getBinOp Plus a b = VInt $ a + b

process :: String -> Value
process s = case parseExp s of
    Right parsed -> eval Map.empty parsed
    Left err -> error $ show err

check :: String -> Type
check s = case parseExp s of
    Right parsed -> typeOf Map.empty parsed
    Left err -> error $ show err

type TypeEnv = Map.Map String Type

typeOf :: TypeEnv -> Term -> Type
typeOf env (TmTrue _) = TyBool
typeOf env (TmFalse _) = TyBool
typeOf env (TmInt _ _) = TyInt
typeOf env (TmVar _ name) = let Just ty = Map.lookup name env in ty
typeOf env (TmAbs _ name ty1 t2) = TyArrow ty1 ty2
    where env' = Map.insert name ty1 env
          ty2 = typeOf env' t2
typeOf env (TmApp info t1 t2) = let ty1 = typeOf env t1
                                    ty2 = typeOf env t2
                                in case ty1 of
    (TyArrow ty1' ty2') -> if ty2 == ty1' then ty1' else error "TmApp param mismatch"
    x -> error $ show x
typeOf env (TmBinOp info _ t1 t2) = if (isIntTy t1) && (isIntTy t2) then TyInt else error "binop mismatch"
    where isIntTy t = (typeOf env t == TyInt)

