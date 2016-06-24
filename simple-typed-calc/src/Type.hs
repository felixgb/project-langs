module Type (
    checkType
) where

import Syntax
import Data.List
import Control.Monad (when)

import qualified Data.Map as Map

type TypeEnv = Map.Map String Type

checkType :: Term -> Either String Type
checkType s = typeOf dataTermEnv s
    where emptyTermEnv = Map.empty
          dataTermEnv = dataToContext emptyTermEnv s

dataToContext :: TypeEnv -> Term -> TypeEnv
dataToContext env (TmDataDec _ name var) = Map.insert name var env
dataToContext env (TmAbs _ _ _ t2) = dataToContext env t2
dataToContext env (TmApp _ t1 t2) = (dataToContext env t1) `Map.union` (dataToContext env t2)
dataToContext env _ = env

simplify :: TypeEnv -> Type -> Type
simplify env (TyDataVar name) = case name `Map.lookup` env of
    Just ty -> ty
    Nothing -> error $ "Can't find type var: " ++ name
simplify _ ty = ty

typeOf :: TypeEnv -> Term -> Either String Type
typeOf env (TmTrue _) = Right TyBool
typeOf env (TmFalse _) = Right TyBool
typeOf env (TmInt _ _) = Right TyInt
typeOf env (TmUnit _) = Right TyUnit
typeOf env (TmDataDec _ _ _) = Right TyUnit
typeOf env var@(TmVar info name) = getVarTy env var
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
typeOf env (TmCase info term cases) = do
    (TyVariant fieldTypes) <- fmap (simplify env) $ typeOf env term
    when (not $ isValidFields cases fieldTypes) (Left "Can't find label in type")
    caseType env fieldTypes cases
typeOf env (TmTag info name term ty) = do
    case simplify env ty of
        (TyVariant fields) -> if tyTi == tyTiExpected 
            then Right ty 
            else Left "Field does not have expected type"
            where (Just tyTiExpected) = name `lookup` fields
                  (Right tyTi) = typeOf env term
        err -> Left "expected variant type"

getVarTy :: TypeEnv -> Term -> Either String Type
getVarTy env (TmVar info name) = case Map.lookup name env of
    Just ty -> Right ty
    Nothing -> Left $ "Can't find var: " ++ name ++ ", env: " ++ (show env)

caseType :: TypeEnv -> [(String, Type)] -> [(String, (String, Term))] -> Either String Type
caseType env fts cases = do
    (tyT1 : restTys) <- mapM tyLookup cases
    if all (== tyT1) restTys then Right tyT1 else Left "Fields do not have the same type"
    where tyLookup (li, (xi, ti)) = do
            let (Just tyTi) = li `lookup` fts
            typeOf (Map.insert xi tyTi env) ti

isValidFields :: [(String, (String, Term))] -> [(String, Type)] -> Bool
isValidFields cases fieldTypes = all associateTypes cases
    where associateTypes (name, (x, ty)) = name `elem` (map fst fieldTypes)

