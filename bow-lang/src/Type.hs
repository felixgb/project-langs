module Type where

import Syntax

import qualified Data.List as L
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.State

type TypeEnv = Map.Map String Type

type Constraint = (Type, Type)

data NameState = NameState {
      count :: Int
    , env :: TypeEnv
    }
                            

type Infer a = (RWST TypeEnv [Constraint] NameState ThrowsError a)

dataToEnv :: TypeEnv -> Expr -> TypeEnv
dataToEnv env expr = case expr of
    (ETaggedUnion _ name ty) -> Map.union (Map.insert name ty env) $ case ty of
        (TyTaggedUnion constrs) -> Map.unions $ fmap (\x -> Map.insert x ty env) $ fmap fst constrs
        (TyRec name (TyTaggedUnion constrs)) -> Map.unions $ fmap (\x -> Map.insert x ty env) $ fmap fst constrs
        _ -> Map.empty
    (ESeq first second) -> Map.union (dte first) (dte second)
        where dte = dataToEnv env
    _ -> env

names :: [String]
names = zipWith (\c n -> c ++ (show n)) (repeat "a") (iterate (+1) 1)

fresh :: Infer Type
fresh = do
    s <- get
    put s { count = count s + 1 }
    return $ TyVar (names !! count s)

current :: Infer String
current = do
    s <- get
    return $ (names !! count s)

initNameState ::TypeEnv -> NameState
initNameState e = NameState { count = 0, env = e }

runInfer :: TypeEnv -> Infer Type -> ThrowsError (Type, [Constraint])
runInfer env m = evalRWST m env (initNameState env)

inferType :: Expr -> ThrowsError (Type, [Constraint])
inferType expr = runInfer dataEnv (recon expr)
    where dataEnv = dataToEnv Map.empty expr

infer :: Expr -> ThrowsError Type
infer ast = do
    (tyExpr, constrs) <- inferType ast
    unified <- unify constrs
    return $ applySubst unified tyExpr

applySubst :: [Constraint] -> Type -> Type
applySubst constrs tyExpr = L.foldl' (\tyS ((TyVar name), tyC2) -> substType name tyC2 tyS) tyExpr constrs

occursIn :: String -> Type -> Bool
occursIn tyName ty = occin ty
    where
        occin (TyFunc tys ty2) = (all occin tys) || occin ty2
        occin TyInt = False
        occin TyBool = False
        occin TyUnit = False
        occin (TyVar s) = s == tyName
        occin (TyTaggedUnion fts) = all occin $ concatMap snd fts


substConstraint :: String -> Type -> [Constraint] -> [Constraint]
substConstraint tyName tyT constrs = fmap (\(tyS1, tyS2) -> (st tyS1, st tyS2)) constrs
    where st ty = substType tyName tyT ty

substType :: String -> Type -> Type -> Type
substType tyName tyT tyS = st tyS
    where
        st (TyFunc tys tyS2) = TyFunc (fmap st tys) (st tyS2)
        st TyInt = TyInt
        st TyBool = TyBool
        st TyUnit = TyUnit
        st (v@(TyVar name)) = if name == tyName then tyT else v
        st (TyTaggedUnion fts) = (TyTaggedUnion (map substed fts))
            where
                substed (name, tys) = (name, map st tys)
        -- st err = error (show err)

unifySubst :: Type -> Type -> [Constraint] -> ThrowsError [Constraint]
unifySubst ty (v@(TyVar tyName)) rest
    | ty == v = unify rest
    | tyName `occursIn` ty = throwError $ ErrCircularUnify tyName ty
    | otherwise = do
        unified <- unify (substConstraint tyName ty rest)
        return $ (v, ty) : unified

unify :: [Constraint] -> ThrowsError [Constraint]
unify constrs = case constrs of
    [] -> return []
    ((tyS, (v@(TyVar tyName))) : rest) -> unifySubst tyS v rest
    ((v@(TyVar tyName), tyT) : rest) -> unifySubst tyT v rest
    ((TyInt, TyInt) : rest) -> unify rest
    ((TyBool, TyBool) : rest) -> unify rest
    ((TyUnit, TyUnit) : rest) -> unify rest
    (((TyFunc tySs tyS2), (TyFunc tyTs tyT2)) : rest) -> do
        let argConstrs = zip tySs tyTs
        unify ((tyS2, tyT2) : argConstrs ++ rest)
    (((TyTaggedUnion fts1), (TyTaggedUnion fts2)) : rest) -> do
        let fts1tys = map snd fts1
        let fts2tys = map snd fts2
        let ftsConstrs = concat $ zipWith (\as bs -> zip as bs) fts1tys fts2tys
        unify (ftsConstrs ++ rest)
    constrs -> throwError $ ErrUnifyUnsolvable constrs

insertIntoEnv :: String -> Type -> Infer ()
insertIntoEnv name ty = do
    s <- get
    let newEnv = Map.insert name ty (env s)
    put s { env = newEnv }
    return ()

lookupEnv :: String -> Infer Type
lookupEnv name = do
    s <- get
    let e = env s
    case Map.lookup name e of
        (Just (TyRec name ty)) -> return ty
        Just ty -> return ty
        Nothing -> throwError $ ErrTyVarNotFound name e

simplify :: Type -> Infer Type
simplify (TyVar name) = do
    x <- lookupEnv name
    simplify x
simplify ty = return ty

recon :: Expr -> Infer Type
recon expr = case expr of
    (EVar _ name) -> lookupEnv name

    (ETag _ name exprs) -> do
        tyTagged <- lookupEnv name
        tysExprs <- mapM recon exprs
        case tyTagged of
            (TyTaggedUnion tags) -> do
                case lookup name tags of
                    (Just types) -> tell $ zip types tysExprs
                    Nothing -> throwError $ ErrTyVarNotFound name (Map.empty)
        return tyTagged

    (ELit _ (LInt _)) -> return TyInt

    (ELit _ (LBool _)) -> return TyBool

    (EUnit _) -> return TyUnit

    (ETaggedUnion _ _ _) -> return TyUnit

    (EDef _ name args body) -> do
        argTys <- replicateM (length args) fresh
        zipWithM (\n ty -> insertIntoEnv n ty) args argTys
        tyBody <- recon body
        insertIntoEnv name $ TyFunc argTys tyBody
        return TyUnit

    (ESeq first second) -> do
        recon first
        recon second

    (EAssign _ name expr) -> do
        tyExpr <- recon expr
        insertIntoEnv name tyExpr
        return TyUnit

    (EFunction name args body scope) -> do
        argTys <- replicateM (length args) fresh
        zipWithM (\n ty -> insertIntoEnv n ty) args argTys
        tyBody <- recon body
        return $ TyFunc argTys tyBody

    (EInvoke _ name argExprs) -> do
        argExprTys <- mapM recon argExprs
        func <- lookupEnv name
        case func of
            (TyFunc argTys bodyTy) -> do
                tell $ zip argTys argExprTys
                return bodyTy
            (TyVar varName) -> do
                newVar <- fresh
                tell $ [(func, (TyFunc argExprTys newVar))]
                insertIntoEnv varName newVar
                return newVar

    (EBinexp info _ left right) -> do
        tyLeft <- recon left
        tyRight <- recon right
        tell $ [(tyLeft, TyInt), (tyRight, TyInt)]
        return TyInt

    (EIf _ cond tr fl) -> do
        tyCond <- recon cond
        tyTr <- recon tr
        tyFl <- recon fl
        tell $ [(tyCond, TyBool), (tyTr, tyFl)]
        return tyFl

    (ECase info expr branches) -> do
        tyExpr <- recon expr
        union <- tagsToUnionsTy branches
        TyTaggedUnion fts <- getFts $ fst $ head branches
        tell $ [(tyExpr, union)]
        branchTys branches fts
        where getFts (ETag _ name _) = lookupEnv name
        -- simpTy <- simplify tyExpr
        -- case simpTy of
        --     (TyTaggedUnion fieldTypes) -> do
        --         caseType fieldTypes branches


tagsToUnionsTy :: [(Expr, Expr)] -> Infer Type
tagsToUnionsTy branches = lookupEnv $ getname $ head branches
    where getname ((ETag _ name _), _) = name

branchTys :: [(Expr, Expr)] -> [(String, [Type])] -> Infer Type
branchTys branches fts = do
    (tyT1 : restTys) <- mapM tyLookup branches
    tell $ map (\ty -> (tyT1, ty)) restTys
    return tyT1
    where
        tyLookup ((ETag _ name args), result) = do
            tys <- lift $ fieldLookup name fts
            zipWithM (\(EVar _ n) ty -> insertIntoEnv n ty) args tys
            recon result

fieldLookup :: String -> [(String, [Type])] -> ThrowsError [Type]
fieldLookup name tys = case lookup name tys of
    Just ty -> return ty
    Nothing -> throwError $ ErrNotInVariantFields name
