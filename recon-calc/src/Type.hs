module Type (
      checkType
    , inferType
) where

import Syntax
import Parser
import Data.List
import Control.Monad (when)
import Control.Monad.Except

import qualified Data.Map as Map

type TypeEnv = Map.Map String Type

checkType :: Term -> Either String Type
checkType s = typeOf dataTermEnv s
    where emptyTermEnv = Map.empty
          dataTermEnv = dataToContext emptyTermEnv s

inferType t = (recon emptyTermEnv names t)
    where emptyTermEnv = Map.empty
          dataTermEnv = dataToContext emptyTermEnv t

foo :: String -> ThrowsError Type
foo inp = case parseExp inp of
    Right ast -> do
        (tyT, constrs) <- inferType ast
        constrs' <- unify constrs
        return $ applySubst constrs' tyT
    Left err -> throwError $ ErrParse

applySubst :: Constraints -> Type -> Type
applySubst constr tyT = foldl' (\tyS ((TyVar name), tyC2) -> substType name tyC2 tyS) tyT constr

names :: [String]
names = zipWith (\c n -> c ++ (show n)) (repeat "a") (iterate (+1) 1)

type Constraints = [(Type, Type)]

occursIn :: String -> Type -> Bool
occursIn tyX (TyArrow tyT1 tyT2) = occursIn tyX tyT1 || occursIn tyX tyT2
occursIn tyX TyInt = False
occursIn tyX TyBool = False
occursIn tyX (TyVar s) = (s == tyX)

substConstraint :: String -> Type -> Constraints -> Constraints
substConstraint tyName tyT constr = map (\(tyS1, tyS2) -> (substType tyName tyT tyS1, substType tyName tyT tyS2)) constr

substType :: String -> Type -> Type -> Type
substType tyName tyT tyS = st tyS
    where
        st (TyArrow tyS1 tyS2) = TyArrow (st tyS1) (st tyS2)
        st TyInt = TyInt
        st TyBool = TyBool
        st (x@(TyVar s)) = if s == tyName then tyT else x

unify :: Constraints -> ThrowsError [(Type, Type)]
unify [] = return $ []
unify ((tyS, (x@(TyVar tyName))) : rest) = if tyS == x then unify rest else
    if occursIn tyName tyS
    then throwError $ ErrUnify "circular constrait 1"
    else do
        unified <- unify (substConstraint tyName tyS rest)
        return $ (x, tyS) : unified
unify ((x@(TyVar tyName), tyT) : rest) = if tyT == x then unify rest else
    if tyName `occursIn` tyT
    then throwError $ ErrUnify $ "circular constraint 2, " ++ (show tyName) ++ " occurs in: " ++ (show tyT) ++ ", constraints: " ++ (show rest)
    else do
        unified <- unify (substConstraint tyName tyT rest)
        return $ (x, tyT) : unified
unify ((TyInt, TyInt) : rest) = unify rest
unify ((TyBool, TyBool) : rest) = unify rest
unify (((TyArrow tyS1 tyS2), (TyArrow tyT1 tyT2)) : rest) = unify ((tyS1, tyT1) : (tyS2, tyT2) : rest)
unify constraints  = throwError $ ErrUnify $ "Unsolvable constraints" ++ (show constraints)

lookupVar :: String -> TypeEnv -> ThrowsError Type
lookupVar ident env = case Map.lookup ident env of
    Just ty -> return ty
    Nothing -> throwError $ ErrTyVar ident

recon :: TypeEnv -> [String] -> Term -> ThrowsError (Type, Constraints)
recon env names (TmVar _ ident) = do
    tyT <- lookupVar ident env
    return (tyT, [])
recon env names (TmAbs info name tyT1 t2) = do
    let env' = Map.insert name tyT1 env
    (tyT2, constraints) <- recon env' names t2
    return ((TyArrow tyT1 tyT2), constraints)
recon env (name : supply) (TmApp info t1 t2) = do
    (tyT1, constr1) <- recon env supply t1
    (tyT2, constr2) <- recon env supply t2
    let newTyVar = TyVar name
    let newConstr = [(tyT1, (TyArrow tyT2 newTyVar))]
    return (newTyVar, newConstr ++ constr2 ++ constr1)
recon env names (TmInt info _) = return (TyInt, [])
recon env names (TmIsZero info t) = do
    (tyT, constrs) <- recon env names t
    return $ (TyBool, (tyT, TyInt) : constrs)
recon env names (TmFalse info) = return (TyBool, [])
recon env names (TmTrue info) = return (TyBool, [])

dataToContext :: TypeEnv -> Term -> TypeEnv
dataToContext env (TmDataDec _ name var) = Map.insert name var env
dataToContext env (TmAbs _ _ _ t2) = dataToContext env t2
dataToContext env (TmApp _ t1 t2) = (dataToContext env t1) `Map.union` (dataToContext env t2)
dataToContext env _ = env

simplify :: TypeEnv -> Type -> Type
simplify env (TyVar name) = case name `Map.lookup` env of
    Just ty -> ty
    Nothing -> error $ "Can't find type var: " ++ name
simplify _ ty = ty

typeOf :: TypeEnv -> Term -> Either String Type
typeOf env (TmTrue _) = Right TyBool
typeOf env (TmFalse _) = Right TyBool
typeOf env (TmInt _ _) = Right TyInt
typeOf env (TmUnit _) = Right TyUnit
typeOf env (TmDataDec _ _ ty) = Right TyUnit
typeOf env var@(TmVar info name) = getVarTy env var
typeOf env (TmAbs _ name ty1 t2) = do
    let env' = Map.insert name ty1 env
    ty2 <- typeOf env' t2
    return $ TyArrow ty1 ty2
typeOf env (TmApp info t1 t2) = do
    ty1 <- typeOf env t1
    ty2 <- fmap (simplify env) $ typeOf env t2
    case ty1 of
        (TyArrow ty1' ty2') -> if ty2 == (simplify env ty1')
            then (Right ty2') 
            else Left $ "Param mismatch: " ++ (show ty2) ++ ", " ++ (show ty1')
        err -> return err
typeOf env (TmBinOp info _ t1 t2) = do
    if (isIntTy t1) && (isIntTy t2) 
    then Right TyInt
    else Left $ "Binary operator mismatch"
        where isIntTy t = (typeOf env t == Right TyInt)
typeOf env (TmCase info term cases) = do
    case fmap (simplify env) $ typeOf env term of
        Right (TyVariant fieldTypes) -> do
            when (not $ isValidFields cases fieldTypes) (Left "Can't find label in type")
            caseType env fieldTypes cases
        Right (TyRecTy x (TyVariant fieldTypes)) -> do
            when (not $ isValidFields cases fieldTypes) (Left "Can't find label in type")
            caseType env fieldTypes cases
        err -> Left $ show err
typeOf env (TmTag info name term ty) = do
    case simplify env ty of
        (TyVariant fields) -> if tyTi == tyTiExpected 
            then Right ty 
            else Left "Field does not have expected type"
            where (Just tyTiExpected) = name `lookup` fields
                  (Right tyTi) = typeOf env term
        (TyRecTy x (TyVariant fields)) -> if tyTi == tyTiExpected
            then Right ty 
            else Left $ "Field does not have expected type: " ++ (show tyTi) ++ ", " ++ (show tyTiExpected)
            where (Just tyTiExpected) = name `lookup` fields
                  (Right tyTi) = typeOf env term
        err -> Left $ "expected variant type but got: " ++ (show err)
typeOf env (TmFold _ tyU tm) = return $ tyU
typeOf env (TmUnfold _ tyU tm) = do
    case simplify env tyU of
        (TyRecTy name tyT) -> typeOf (Map.insert name tyU env) tm
typeOf env (TmPair _ tms) = do
    termTys <- mapM (typeOf env) tms
    let simplified = map (simplify env) termTys
    return $ TyProd simplified
typeOf env (TmProj _ t idx) = do
    tyT <- typeOf env t
    case simplify env tyT of
        (TyProd tys) -> return $ tys !! idx
        err -> Left $ "Not a product type: " ++ (show t)

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

