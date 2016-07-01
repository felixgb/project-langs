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

initNameState :: NameState
initNameState = NameState { count = 0 }

runInfer :: TypeEnv -> Infer Type -> ThrowsError (Type, Constraints)
runInfer env m = runExcept $ evalRWST m env initNameState

inferType :: Term -> ThrowsError (Type, Constraints)
inferType t = runInfer emptyTypeEnv (recon t)
    where emptyTypeEnv = Map.empty
          dataTypeEnv = dataToContext dataTypeEnv t

parseAndInfer :: String -> ThrowsError Type
parseAndInfer inp = case parseExp inp of
    Right ast -> do
        (tyT, constrs) <- inferType ast
        constrs' <- unify constrs
        return $ applySubst constrs' tyT
    Left err -> throwError $ ErrParse

applySubst :: Constraints -> Type -> Type
applySubst constr tyT = foldl' (\tyS ((TyVar name), tyC2) -> substType name tyC2 tyS) tyT constr

occursIn :: String -> Type -> Bool
occursIn tyName ty = occin ty
    where 
        occin (TyArrow tyT1 tyT2) = occin tyT1 || occin tyT2
        occin TyInt = False
        occin TyBool = False
        occin (TyVar s) = s == tyName

substConstraint :: String -> Type -> Constraints -> Constraints
substConstraint tyName tyT constr = map (\(tyS1, tyS2) -> (substType tyName tyT tyS1, substType tyName tyT tyS2)) constr

substType :: String -> Type -> Type -> Type
substType tyName tyT tyS = st tyS
    where
        st (TyArrow tyS1 tyS2) = TyArrow (st tyS1) (st tyS2)
        st TyInt = TyInt
        st TyBool = TyBool
        st (x@(TyVar s)) = if s == tyName then tyT else x

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
unify ((TyBool, TyBool) : rest) = unify rest
unify (((TyArrow tyS1 tyS2), (TyArrow tyT1 tyT2)) : rest) = unify ((tyS1, tyT1) : (tyS2, tyT2) : rest)
unify constraints  = throwError $ ErrUnifyUnsolvable constraints

insertIntoEnv :: String -> Type -> Infer Type -> Infer Type
insertIntoEnv x ty m = do
    let newEnv = Map.insert x ty
    local newEnv m

lookupEnv :: String -> Infer Type
lookupEnv x = do
    env <- ask
    case Map.lookup x env of
        Just ty -> return ty
        Nothing -> throwError $ ErrTyVar x

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
    tyT1 <- recon t1
    tyT2 <- recon t2
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

dataToContext :: TypeEnv -> Term -> TypeEnv
dataToContext env (TmDataDec _ name var) = Map.insert name var env
dataToContext env (TmAbs _ _ _ t2) = dataToContext env t2
dataToContext env (TmApp _ t1 t2) = (dataToContext env t1) `Map.union` (dataToContext env t2)
dataToContext env _ = env
-- 
-- simplify :: TypeEnv -> Type -> Type
-- simplify env (TyVar name) = case name `Map.lookup` env of
--     Just ty -> ty
--     Nothing -> error $ "Can't find type var: " ++ name
-- simplify _ ty = ty
-- 
-- typeOf :: TypeEnv -> Term -> Either String Type
-- typeOf env (TmTrue _) = Right TyBool
-- typeOf env (TmFalse _) = Right TyBool
-- typeOf env (TmInt _ _) = Right TyInt
-- typeOf env (TmUnit _) = Right TyUnit
-- typeOf env (TmDataDec _ _ ty) = Right TyUnit
-- typeOf env var@(TmVar info name) = getVarTy env var
-- typeOf env (TmAbs _ name ty1 t2) = do
--     let env' = Map.insert name ty1 env
--     ty2 <- typeOf env' t2
--     return $ TyArrow ty1 ty2
-- typeOf env (TmApp info t1 t2) = do
--     ty1 <- typeOf env t1
--     ty2 <- fmap (simplify env) $ typeOf env t2
--     case ty1 of
--         (TyArrow ty1' ty2') -> if ty2 == (simplify env ty1')
--             then (Right ty2') 
--             else Left $ "Param mismatch: " ++ (show ty2) ++ ", " ++ (show ty1')
--         err -> return err
-- typeOf env (TmBinOp info _ t1 t2) = do
--     if (isIntTy t1) && (isIntTy t2) 
--     then Right TyInt
--     else Left $ "Binary operator mismatch"
--         where isIntTy t = (typeOf env t == Right TyInt)
-- typeOf env (TmCase info term cases) = do
--     case fmap (simplify env) $ typeOf env term of
--         Right (TyVariant fieldTypes) -> do
--             when (not $ isValidFields cases fieldTypes) (Left "Can't find label in type")
--             caseType env fieldTypes cases
--         Right (TyRecTy x (TyVariant fieldTypes)) -> do
--             when (not $ isValidFields cases fieldTypes) (Left "Can't find label in type")
--             caseType env fieldTypes cases
--         err -> Left $ show err
-- typeOf env (TmTag info name term ty) = do
--     case simplify env ty of
--         (TyVariant fields) -> if tyTi == tyTiExpected 
--             then Right ty 
--             else Left "Field does not have expected type"
--             where (Just tyTiExpected) = name `lookup` fields
--                   (Right tyTi) = typeOf env term
--         (TyRecTy x (TyVariant fields)) -> if tyTi == tyTiExpected
--             then Right ty 
--             else Left $ "Field does not have expected type: " ++ (show tyTi) ++ ", " ++ (show tyTiExpected)
--             where (Just tyTiExpected) = name `lookup` fields
--                   (Right tyTi) = typeOf env term
--         err -> Left $ "expected variant type but got: " ++ (show err)
-- typeOf env (TmFold _ tyU tm) = return $ tyU
-- typeOf env (TmUnfold _ tyU tm) = do
--     case simplify env tyU of
--         (TyRecTy name tyT) -> typeOf (Map.insert name tyU env) tm
-- typeOf env (TmPair _ tms) = do
--     termTys <- mapM (typeOf env) tms
--     let simplified = map (simplify env) termTys
--     return $ TyProd simplified
-- typeOf env (TmProj _ t idx) = do
--     tyT <- typeOf env t
--     case simplify env tyT of
--         (TyProd tys) -> return $ tys !! idx
--         err -> Left $ "Not a product type: " ++ (show t)
-- 
-- getVarTy :: TypeEnv -> Term -> Either String Type
-- getVarTy env (TmVar info name) = case Map.lookup name env of
--     Just ty -> Right ty
--     Nothing -> Left $ "Can't find var: " ++ name ++ ", env: " ++ (show env)
-- 
-- caseType :: TypeEnv -> [(String, Type)] -> [(String, (String, Term))] -> Either String Type
-- caseType env fts cases = do
--     (tyT1 : restTys) <- mapM tyLookup cases
--     if all (== tyT1) restTys then Right tyT1 else Left "Fields do not have the same type"
--     where tyLookup (li, (xi, ti)) = do
--             let (Just tyTi) = li `lookup` fts
--             typeOf (Map.insert xi tyTi env) ti
-- 
-- isValidFields :: [(String, (String, Term))] -> [(String, Type)] -> Bool
-- isValidFields cases fieldTypes = all associateTypes cases
--     where associateTypes (name, (x, ty)) = name `elem` (map fst fieldTypes)
-- 
