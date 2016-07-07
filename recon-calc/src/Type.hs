module Type where

import Syntax
import Parser

import Data.List
import qualified Data.Map as Map

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS.Strict

type TypeEnv = Map.Map String Type

type Constraints = [(Type, Type)]

data NameState = NameState { count :: Int }

type Infer a = (RWST
    TypeEnv
    Constraints
    NameState
    (Except CalcError)
    a)

names :: [String]
names = zipWith (\c n -> c ++ (show n)) (repeat "a") (iterate (+1) 1)

fresh :: Infer Type
fresh = do
    s <- get
    put s { count = count s + 1 }
    return $ TyVar (names !! count s)

-- Gets the current fresh var name, kinda a bodge, need to check...
current :: Infer String
current = do
    s <- get
    return $ (names !! count s)

initNameState :: NameState
initNameState = NameState { count = 0 }

runInfer :: TypeEnv -> Infer Type -> ThrowsError (Type, Constraints)
runInfer env m = evalRWST m env initNameState

inferType :: Term -> ThrowsError (Type, Constraints)
inferType t = runInfer dataTypeEnv (recon t)
    where emptyTypeEnv = Map.empty
          dataTypeEnv = dataToContext emptyTypeEnv t

dataToContext :: TypeEnv -> Term -> TypeEnv
dataToContext env (TmDataDec _ name var) = Map.insert name var env
dataToContext env (TmAbs _ _ _ t2) = dataToContext env t2
dataToContext env (TmApp _ t1 t2) = (dataToContext env t1) `Map.union` (dataToContext env t2)
dataToContext env _ = env

infer :: Term -> ThrowsError Type
infer ast = do
    (tyT, constrs) <- inferType ast
    constrs' <- unify constrs
    return $ applySubst constrs' tyT

applySubst :: Constraints -> Type -> Type
applySubst constr tyT = foldl' (\tyS ((TyVar name), tyC2) -> substType name tyC2 tyS) tyT constr

occursIn :: String -> Type -> Bool
occursIn tyName ty = occin ty
    where 
        occin (TyArrow tyT1 tyT2) = occin tyT1 || occin tyT2
        occin TyInt = False
        occin TyBool = False
        occin (TyVar s) = s == tyName
        occin (TyProd x) = all occin x

substConstraint :: String -> Type -> Constraints -> Constraints
substConstraint tyName tyT constr = map (\(tyS1, tyS2) -> (substType tyName tyT tyS1, substType tyName tyT tyS2)) constr

substType :: String -> Type -> Type -> Type
substType tyName tyT tyS = st tyS
    where
        st (TyArrow tyS1 tyS2) = TyArrow (st tyS1) (st tyS2)
        st TyInt = TyInt
        st TyBool = TyBool
        st TyUnit = TyUnit
        st (x@(TyVar s)) = if s == tyName then tyT else x
        st (TyProd tys) = TyProd (map st tys)
        st err = error (show err)

unifySubst :: Type -> Type -> Constraints -> ThrowsError [(Type, Type)]
unifySubst ty (x@(TyVar tyName)) rest 
    | ty == x = unify rest
    | tyName `occursIn` ty = throwError $ ErrUnifyCircular tyName ty
    | otherwise = do
        unified <- unify (substConstraint tyName ty rest)
        return $ (x, ty) : unified

unify :: Constraints -> ThrowsError [(Type, Type)]
unify [] = return $ []
unify ((tyS, (x@(TyVar tyName))) : rest) = unifySubst tyS x rest
unify ((x@(TyVar tyName), tyT) : rest) = unifySubst tyT x rest
unify ((TyInt, TyInt) : rest) = unify rest
unify ((TyUnit, TyUnit) : rest) = unify rest
unify ((TyBool, TyBool) : rest) = unify rest
unify (((TyArrow tyS1 tyS2), (TyArrow tyT1 tyT2)) : rest) = unify ((tyS1, tyT1) : (tyS2, tyT2) : rest)
unify (((TyProd tys1), (TyProd tys2)) : rest) = unify $ (zip tys1 tys2) ++ rest
unify constraints  = throwError $ ErrUnifyUnsolvable constraints

insertIntoEnv :: String -> Type -> Infer Type -> Infer Type
insertIntoEnv x ty m = do
    let newEnv = Map.insert x ty
    local newEnv m

lookupEnv :: String -> Infer Type
lookupEnv name = do
    env <- ask
    case Map.lookup name env of
        Just ty -> return ty
        Nothing -> throwError $ ErrTyVar name (Map.assocs env)

simplify :: Type -> Infer Type
simplify (TyVar name) = do
    x <- lookupEnv name
    simplify x
simplify ty = return ty

recon :: Term -> Infer Type
recon (TmVar _ ident) = do
    tyT <- lookupEnv ident
    return tyT
recon (TmAbs info name (Just tyT1) t2) = do
    tyT2 <- insertIntoEnv name tyT1 (recon t2)
    return $ TyArrow tyT1 tyT2
recon (TmAbs info name Nothing t2) = do
    tyT1 <- fresh
    tyT2 <- insertIntoEnv name tyT1 (recon t2)
    return $ TyArrow tyT1 tyT2
recon (TmApp info t1 t2) = do
    tyT2 <- recon t2
    ty2Var <- current
    tyT1 <- insertIntoEnv ty2Var tyT2 (recon t1)
    newTyVar <- fresh
    tell $ [(tyT1, (TyArrow tyT2 newTyVar))]
    return newTyVar
recon (TmInt info _) = return TyInt
recon (TmIsZero info t) = do
    tyT <- recon t
    tell $ [(tyT, TyInt)]
    return TyBool
recon (TmTrue info) = return TyBool
recon (TmFalse info) = return TyBool
recon (TmUnit info) = return TyUnit
recon (TmDataDec info _ _) = return TyUnit
recon (TmBinOp info _ t1 t2) = do
    ty1 <- recon t1
    ty2 <- recon t2
    tell $ [(ty1, TyInt), (ty2, TyInt)]
    return TyInt
recon (TmIf info cond tThen tElse) = do
    tyCond <- recon cond
    tyThen <- recon tThen
    tyElse <- recon tElse
    tell $ [(tyCond, TyBool), (tyThen, tyElse)]
    return tyElse
recon (TmPair info terms) = do
    tyTerms <- mapM recon terms
    return $ TyProd tyTerms
recon (TmProj info term idx) = do
    tyT <- recon term
    simpTy <- simplify tyT
    case simpTy of
        (TyProd tys) -> do
            tell $ [(tyT, simpTy)]
            return $ tys !! idx
        err -> throwError $ ErrNotProduct err
recon (TmCase info term cases) = do
    tyT <- recon term
    simpTy <- simplify tyT
    case simpTy of
        (TyVariant fieldTypes) -> do
            when (not $ isValidFields cases fieldTypes) (throwError $ ErrMissingLabel "Can't find label in type")
            caseType fieldTypes cases
        (TyRecTy _ (TyVariant fieldTypes)) -> do
            when (not $ isValidFields cases fieldTypes) (throwError $ ErrMissingLabel "Can't find label in type")
            caseType fieldTypes cases
        err -> throwError $ ErrNotVariant err
recon (TmTag info name term ty) = do
    simpTy <- simplify ty
    case simpTy of
        (TyVariant fields) -> do
            tyTi <- lift $ fieldLookup name fields
            tyTiExpected <- recon term
            if tyTi == tyTiExpected
            then return ty
            else throwError $ ErrFieldMismatch "Tag does not have the right fields"
        (TyRecTy _ (TyVariant fields)) -> do
            tyTi <- lift $ fieldLookup name fields
            tyTiExpected <- recon term
            if tyTi == tyTiExpected
            then return ty
            else throwError $ ErrFieldMismatch "Tag does not have the right fields"
        err -> throwError $ ErrNotVariant err
recon (TmFold info tyU tm) = return $ tyU
recon (TmUnfold info tyU tm) = do
    simpTy <- simplify tyU
    case simpTy of
        (TyRecTy name tyT) -> insertIntoEnv name tyU (recon tm)
        err -> throwError $ ErrNotRecTy err

fieldLookup :: String -> [(String, Type)] -> Except CalcError Type
fieldLookup name tys = case name `lookup` tys of
    Just ty -> return ty
    Nothing -> throwError $ ErrMissingLabel "Can't find label in type"

caseType :: [(String, Type)] -> [(String, (String, Term))] -> Infer Type
caseType fts cases = do
    env <- ask
    (tyT1 : restTys) <- mapM tyLookup cases
    if all (== tyT1) restTys 
    then return tyT1
    else throwError $ ErrFieldMismatch "Fields do not have the same type"
    where tyLookup (li, (xi, ti)) = do
            tyTi <- lift $ fieldLookup li fts
            insertIntoEnv xi tyTi (recon ti)

isValidFields :: [(String, (String, Term))] -> [(String, Type)] -> Bool
isValidFields cases fieldTypes = all associateTypes cases
    where associateTypes (name, (x, ty)) = name `elem` (map fst fieldTypes)
