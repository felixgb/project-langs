module Type (
    checkType
) where

import Syntax

import qualified Data.Map as Map

type TypeEnv = Map.Map String Type

checkType :: Term -> Either String Type
checkType s = typeOf emptyTermEnv s
    where emptyTermEnv = Map.empty

typeOf :: TypeEnv -> Term -> Either String Type
typeOf env (TmTrue _) = Right TyBool
typeOf env (TmFalse _) = Right TyBool
typeOf env (TmInt _ _) = Right TyInt
typeOf env (TmVar info name) = case Map.lookup name env of
    Just ty -> Right ty
    Nothing -> Left $ "Can't find var " ++ name ++ " at " ++ (show info)
typeOf env (TmAbs _ name ty1 t2) = do
    let env' = Map.insert name ty1 env
    ty2 <- typeOf env' t2
    return $ TyArrow ty1 ty2
typeOf env (TmApp info t1 t2) = do
    ty1 <- typeOf env t1
    ty2 <- typeOf env t2
    case ty1 of
        (TyArrow ty1' ty2') -> if ty2 == ty1' then (Right ty1') else Left $ "Param mismatch"
        err -> return err
typeOf env (TmBinOp info _ t1 t2) = do
    if (isIntTy t1) && (isIntTy t2) 
    then Right TyInt
    else Left $ "Binary operator mismatch"
        where isIntTy t = (typeOf env t == Right TyInt)
