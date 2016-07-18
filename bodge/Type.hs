
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type where

import Syntax

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.State
import Control.Monad.Identity

type TypeEnv = Map.Map String Scheme

type Constraint = (Type, Type)

data NameState = NameState {
      count :: Int
    , env :: TypeEnv
    }
                            

type Infer a = (RWST TypeEnv [Constraint] NameState ThrowsError a)

class Substitutable a where
    apply :: Subst -> a -> a
    freeTyVars :: a -> Set.Set Type

instance Substitutable Type where
    apply _ TyInt = TyInt
    apply _ TyBool = TyBool
    apply _ TyUnit = TyUnit
    apply s (TyFunc tys tyS2) = TyFunc (fmap (apply s) tys) (apply s tyS2)
    apply (Subst s) t@(TyVar name) = Map.findWithDefault t t s
    apply s (TyTaggedUnion fts) = (TyTaggedUnion (map substed fts))
            where
                substed (name, tys) = (name, map (apply s) tys)

    freeTyVars TyInt = Set.empty
    freeTyVars TyBool = Set.empty
    freeTyVars TyUnit = Set.empty
    freeTyVars (TyVar a) = Set.singleton (TyVar a)
    freeTyVars (TyFunc tys tyS2) = Set.unions $ (freeTyVars tyS2) : (fmap freeTyVars tys)
    freeTyVars (TyTaggedUnion fts) = error "tagged union not implemented"

instance Substitutable Scheme where
    apply (Subst s) (Forall tyVars t) = Forall tyVars $ apply s' t
        where s' = Subst $ foldr Map.delete s tyVars
    freeTyVars (Forall tyVars t) = freeTyVars t `Set.difference` Set.fromList tyVars

instance Substitutable a => Substitutable [a] where
    apply = map . apply
    freeTyVars = foldr (Set.union .freeTyVars) Set.empty

instance Substitutable (Map.Map String Scheme) where
    apply s env = Map.map (apply s) env
    freeTyVars env = freeTyVars $ Map.elems env

instance Substitutable Constraint where
    apply s (t1, t2) = (apply s t1, apply s t2)
    freeTyVars (t1, t2) = freeTyVars t1 `Set.union` freeTyVars t2

-- 
-- dataToEnv :: TypeEnv -> Expr -> TypeEnv
-- dataToEnv env expr = case expr of
--     (ETaggedUnion _ name ty) -> Map.union (Map.insert name ty env) $ case ty of
--         (TyTaggedUnion constrs) -> Map.unions $ fmap (\ x -> Map.insert x ty env) $ fmap fst constrs
--         (TyRec name (TyTaggedUnion constrs)) -> Map.unions $ fmap (\ x -> Map.insert x ty env) $ fmap fst constrs
--         _ -> Map.empty
--     (ESeq first second) -> Map.union (dte first) (dte second)
--         where dte = dataToEnv env
--     _ -> env
-- 
names :: [String]
names = zipWith (\ c n -> c ++ (show n)) (repeat "a") (iterate (+1) 1)

fresh :: Infer Type
fresh = do
    s <- get
    put s { count = count s + 1 }
    return $ TyVar (names !! count s)

initNameState ::TypeEnv -> NameState
initNameState e = NameState { count = 0, env = e }

runInfer :: TypeEnv -> Infer Type -> ThrowsError (Type, [Constraint])
runInfer env m = evalRWST m env (initNameState env)

inferType :: Expr -> ThrowsError (Type, [Constraint])
inferType expr = runInfer Map.empty (recon expr)
    --where dataEnv = dataToEnv Map.empty expr

infer :: Expr -> ThrowsError Scheme
infer ex = case inferExpr Map.empty ex of
    Left err -> throwError err
    Right ty -> return ty

constraintsExpr :: Expr -> ThrowsError ([Constraint], Subst, Type, Scheme)
constraintsExpr ex = do
    (ty, constrs) <- runInfer Map.empty (recon ex)
    case runSolve constrs of
        Left err -> throwError err
        Right subst -> return $ (constrs, subst, ty, (closeOver $ apply subst ty))

-- 
-- infer :: Expr -> ThrowsError Type
-- infer ast = do
--     (tyExpr, constrs) <- inferExpr ast
--     unified <- unify constrs
--     return $ applySubst unified tyExpr
-- 
instantiate :: Scheme -> Infer Type
instantiate (Forall xs ty) = do
    names <- mapM (\ _ -> fresh) xs
    let s = Subst $ Map.fromList $ zip xs names
    return $ apply s ty

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall xs t
    where xs = Set.toList $ freeTyVars t `Set.difference` freeTyVars env

inferExpr :: TypeEnv -> Expr -> Either LangErr Scheme
inferExpr env ex = case runExcept $ runInfer env (recon ex) of
    Left err -> Left err
    Right (ty, constrs) -> case runSolve constrs of
        Left err -> Left err
        Right subst -> Right $ closeOver $ apply subst ty

closeOver :: Type -> Scheme
closeOver = normalize . generalize Map.empty

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
    where
        ord = zip (L.nub $ fv body) (map TyVar names)
        fv (TyVar a) = [a]
        fv (TyFunc as b) = (fv b) ++ (concatMap fv as)
        fv _ = []
        normtype (TyFunc as b) = TyFunc (map normtype as) (normtype b)
        normtype (TyVar a) = case lookup a ord of
            Just x -> TyVar a
            Nothing -> error "ty var not in sig"
        normtype TyInt = TyInt
        normtype TyBool = TyBool

type Unifier = (Subst, [Constraint])

type Solve a = ExceptT LangErr Identity a

newtype Subst = Subst (Map.Map Type Type)
    deriving (Eq, Ord, Show, Monoid)

compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

runSolve :: [Constraint] -> Either LangErr Subst
runSolve constrs = runIdentity $ runExceptT $ solver st
    where st = (mempty, constrs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return mempty
unifyMany (t1 : ts1) (t2 : ts2) = do
    su1 <- unifies t1 t2
    su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
    return (su2 `compose` su1)
-- unifyMany t1 t2 = throwError $ ErrUnifyUnsolvable [] -- Fix lang error
unifyMany t1 t2 = throwError $ ErrUnify t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return mempty
unifies (TyVar name) t = name `bind` t
unifies t (TyVar name) = name `bind` t
unifies (TyFunc tySs tyS2) (TyFunc tyTs tyT2) = unifyMany (tyS2 : tySs) (tyT2 : tyTs)
--unifies t1 t2 = throwError $ ErrUnifyUnsolvable []
unifies t1 t2 = throwError $ ErrUnify [t1] [t2]

solver :: Unifier -> Solve Subst
solver (su, constrs) = case constrs of
    [] -> return su
    ((t1, t2) : cs0) -> do
        su1 <- unifies t1 t2
        solver (su1 `compose` su, (apply su1 cs0))

bind :: String -> Type -> Solve Subst
bind a t
    | t == TyVar a = return mempty
    | occursCheck a t = throwError $ ErrCircularUnify a t
    | otherwise = return $ Subst $ Map.singleton (TyVar a) t

occursCheck :: Substitutable a => String -> a -> Bool
occursCheck a t = (TyVar a) `Set.member` freeTyVars t

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

applySubst :: [Constraint] -> Type -> Type
applySubst constrs tyExpr = L.foldl' (\ tyS ((TyVar name), tyC2) -> substType name tyC2 tyS) tyExpr constrs

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
substConstraint tyName tyT constrs = fmap (\ (tyS1, tyS2) -> (st tyS1, st tyS2)) constrs
    where st ty = substType tyName tyT ty

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
        let ftsConstrs = concat $ zipWith (\ as bs -> zip as bs) fts1tys fts2tys
        unify (ftsConstrs ++ rest)
    constrs -> throwError $ ErrUnifyUnsolvable constrs

insertIntoEnv :: String -> Scheme -> Infer ()
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
        --(Just (TyRec name ty)) -> return ty
        Just ty -> do
            t <- instantiate ty
            return t
        Nothing -> throwError $ ErrTyVarNotFound name e

inEnv :: String -> Scheme -> Infer ()
inEnv name scheme = do
    s <- get
    let oldEnv = env s
    let newEnv = Map.delete name oldEnv
    insertIntoEnv name scheme

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
        e <- fmap env get
        funcTy <- fresh
        inEnv name (generalize e funcTy)
        argTys <- replicateM (length args) fresh
        zipWithM (\ n ty -> inEnv n (generalize e ty)) args argTys
        tyBody <- recon body
        inEnv name (generalize e $ TyFunc argTys tyBody)
        return TyUnit

    (ESeq first second) -> do
        recon first
        recon second

    (EAssign _ name expr) -> do
        tyExpr <- recon expr
        e <- fmap env get
        inEnv name (generalize e tyExpr)
        return TyUnit

    (EFunction name args body scope) -> do
        e <- fmap env get
        funcTy <- fresh
        inEnv name (generalize e funcTy)
        argTys <- replicateM (length args) fresh
        zipWithM (\ n ty -> inEnv n (generalize e ty)) args argTys
        tyBody <- recon body
        inEnv name (generalize e $ TyFunc argTys tyBody)
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
                -- insertIntoEnv varName (Forall [] newVar)
                return newVar

    (EBinexp info op left right) -> do
        tyLeft <- recon left
        tyRight <- recon right
        tell $ [(tyLeft, TyInt), (tyRight, TyInt)]
        return $ opTypes op

    (EIf _ cond tr fl) -> do
        tyCond <- recon cond
        tyTr <- recon tr
        tyFl <- recon fl
        tell $ [(tyCond, TyBool), (tyTr, tyFl)]
        return tyFl
-- 
--     (ECase info expr branches) -> do
--         tyExpr <- recon expr
--         union <- tagsToUnionsTy branches
--         TyTaggedUnion fts <- getFts $ fst $ head branches
--         tell $ [(tyExpr, union)]
--         branchTys branches fts
--         where getFts (ETag _ name _) = lookupEnv name
-- 
opTypes :: Op -> Type
opTypes Plus = TyInt
opTypes Times = TyInt
opTypes Minus = TyInt
opTypes Equal = TyBool
-- 
-- tagsToUnionsTy :: [(Expr, Expr)] -> Infer Type
-- tagsToUnionsTy branches = lookupEnv $ getname $ head branches
--     where getname ((ETag _ name _), _) = name
-- 
-- branchTys :: [(Expr, Expr)] -> [(String, [Type])] -> Infer Type
-- branchTys branches fts = do
--     (tyT1 : restTys) <- mapM tyLookup branches
--     tell $ map (\ ty -> (tyT1, ty)) restTys
--     return tyT1
--     where
--         tyLookup ((ETag _ name args), result) = do
--             tys <- lift $ fieldLookup name fts
--             zipWithM (\ (EVar _ n) ty -> insertIntoEnv n ty) args tys
--             recon result
-- 
-- fieldLookup :: String -> [(String, [Type])] -> ThrowsError [Type]
-- fieldLookup name tys = case lookup name tys of
--     Just ty -> return ty
--     Nothing -> throwError $ ErrNotInVariantFields name
