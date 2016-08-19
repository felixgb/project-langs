{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type where

import Syntax

import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.State
import Control.Monad.Identity

type TypeEnv = Map.Map String Type

type Constraint = (Type, Type)

class Substitutable a where
    apply :: Subst -> a -> a
    freeTyVars :: a -> Set.Set Type

instance Substitutable Type where
    apply _ TyInt = TyInt
    apply _ TyBool = TyBool
    apply _ TyUnit = TyUnit
    apply s (TyRecord fields) = TyRecord $ zip (map fst fields) (map (apply s . snd) fields)
    apply s (TyFunc tys tyS2) = TyFunc (apply s tys) (apply s tyS2)
    apply (Subst s) t@(TyVar name) = Map.findWithDefault t t s
    apply s (TyApp (TyAbs param body) actual) = apply s (apply newSubst body)
        where newSubst = Subst $ Map.fromList [(param, actual)]
    apply s (TyApp func param) = TyApp func (apply s param)
    apply s (TyAbs param body) = TyAbs param (apply s body)
    apply (Subst s) (Forall tyVars t) = Forall tyVars $ apply s' t
        where s' = Subst $ foldr Map.delete s tyVars
    apply s (TyTaggedUnion fts) = (TyTaggedUnion (map substed fts))
            where
                substed (name, tys) = (name, map (apply s) tys)
    apply _ err = error $ show err

    freeTyVars TyInt = Set.empty
    freeTyVars TyBool = Set.empty
    freeTyVars TyUnit = Set.empty
    freeTyVars (TyRecord fields) = freeTyVars $ map snd fields
    freeTyVars (TyVar a) = Set.singleton (TyVar a)
    freeTyVars (TyFunc tys tyS2) = Set.unions $ (freeTyVars tyS2) : (fmap freeTyVars tys)
    freeTyVars (Forall tyVars t) = freeTyVars t `Set.difference` Set.fromList tyVars
    freeTyVars (TyTaggedUnion fts) = Set.unions $ map (freeTyVars . snd) fts
    freeTyVars (TyAbs param body) = freeTyVars body `Set.difference` freeTyVars param
    freeTyVars (TyApp func param) = freeTyVars func `Set.union` freeTyVars param
    freeTyVars err = error $ show err

instance Substitutable a => Substitutable [a] where
    apply = map . apply
    freeTyVars = foldr (Set.union . freeTyVars) Set.empty

instance Substitutable (Map.Map String Type) where
    apply s env = Map.map (apply s) env
    freeTyVars env = freeTyVars $ Map.elems env

instance Substitutable Constraint where
    apply s (t1, t2) = (apply s t1, apply s t2)
    freeTyVars (t1, t2) = freeTyVars t1 `Set.union` freeTyVars t2

names :: [String]
names = zipWith (\c n -> c ++ (show n)) (repeat "a") (iterate (+1) 1)

constraintsExpr :: Expr -> ThrowsError ([Constraint], Subst, Type, Type)
constraintsExpr ex = do
    ((ty, _), (InferState _ cs)) <- runMkConstr ex
    -- (ty, constrs) <- runInfer Map.empty (recon ex)
    case runSolve cs of
        Left err -> throwError err
        Right subst -> return $ (cs, subst, ty, (closeOver $ apply subst ty))

closeOver :: Type -> Type
closeOver = normalize . generalize Map.empty

normalize :: Type -> Type
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
    where
        ord = zip (L.nub $ fv body) (map TyVar names)
        fv (TyVar a) = [a]
        fv (TyFunc as b) = (fv b) ++ (concatMap fv as)
        fv _ = []
        normtype (TyFunc as b) = TyFunc (map normtype as) (normtype b)
        normtype (TyVar a) = (TyVar a) -- case lookup a ord of
            -- Just x -> TyVar a
            -- Nothing -> error $ a ++ " not in " ++ show ord
        normtype TyInt = TyInt
        normtype TyBool = TyBool
        normtype TyUnit = TyUnit
        normtype (TyTaggedUnion fields) = TyTaggedUnion fields
        normtype (TyAbs param body) = (TyAbs param body)
        normtype (TyApp func param) = TyApp (normtype func) (normtype param)
        normtype err = error $ show "invalid form: " ++ show err

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
unifyMany t1 t2 = throwError $ ErrUnify t1 t2

-- Value constuctors should take the correct number of arguments
-- Testing for type equivalence, also evaluating type expressions
unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return mempty
unifies (TyVar name) t = name `bind` t
unifies t (TyVar name) = name `bind` t
unifies (TyFunc tySs tyS2) (TyFunc tyTs tyT2) = unifyMany (tyS2 : tySs) (tyT2 : tyTs)
unifies (TyApp (TyAbs param body) actual) t2 = unifies (apply newSubst body) t2
    where newSubst = Subst $ Map.singleton param actual
unifies t1 (TyApp (TyAbs param body) actual) = unifies t1 (apply newSubst body) 
    where newSubst = Subst $ Map.singleton param actual
unifies (TyApp func1 param1) (TyApp func2 param2) 
    | func1 == func2 = unifies param1 param2
unifies (TyTaggedUnion fts1) (TyTaggedUnion fts2) = unifyMany (concatMap snd fts1) (concatMap snd fts2)
unifies (TyAbs param1 body1) (TyAbs param2 body2) = unifyMany [param1, body1] [param2, body2]
unifies t1 t2 = throwError $ ErrUnify [t1] [t2]

type Unifier = (Subst, [Constraint])

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

data InferState a = InferState {
      nameIndex :: Int
    , constraints :: [(a, a)]
} deriving (Show)

type Infer a b = StateT (InferState b) ThrowsError a

initInfer :: InferState a
initInfer = InferState { nameIndex = 0, constraints = [] }

type KindEnv = Map.Map String Kind

runKindOf :: Type -> ThrowsError ((Kind, KindEnv), (InferState Kind))
runKindOf ty = runStateT (kindOf ty Map.empty) initInfer

constraintsTy :: Type -> ThrowsError ([KindConstr], KSubst, Kind, Kind)
constraintsTy (Forall _ ty) = do
    ((ty, _), (InferState _ cs)) <- runKindOf ty
    -- (ty, constrs) <- runInfer Map.empty (recon ex)
    case runKSolve cs of
        Left err -> throwError err
        Right subst -> return $ (cs, subst, ty, (replaceKVars $ kapply subst ty))

replaceKVars :: Kind -> Kind
replaceKVars (KVar _) = KStar
replaceKVars (KArr k1 k2) = KArr (replaceKVars k1) (replaceKVars k2)
replaceKVars KStar = KStar

freshKVar :: Infer Kind Kind
freshKVar = do
    s <- get
    put s { nameIndex = nameIndex s + 1 }
    return $ KVar (names !! nameIndex s)

kindOf :: Type -> KindEnv -> Infer (Kind, KindEnv) Kind
kindOf ty env = case ty of

    TyInt -> return (KStar, env)
    TyBool -> return (KStar, env)
    TyUnit -> return (KStar, env)
    TyFunc{} -> return (KStar, env)

    (TyVar name) -> do
        ty <- lift $ lookupEnv name env
        return (ty, env)

    (TyAbs (TyVar name) body) -> do
        kv <- freshKVar
        let env' = Map.insert name kv env
        (bodyTy, env'') <- kindOf body env'
        return (KArr kv bodyTy, env)

    (TyApp func param) -> do
        (funcTy, env') <- kindOf func env
        (paramTy, env') <- kindOf param env
        kv <- freshKVar
        addConstr (funcTy, KArr paramTy kv)
        return (kv, env)

    (TyTaggedUnion branches) -> do
        forM branches (\(_, tys) -> do
            mapM (\t -> kindOf t env) tys)
        return (KStar, env)

    err -> error $ show err

getBody :: Type -> Type
getBody (TyAbs param body) = getBody body
getBody other = other

evalType :: Type -> TypeEnv -> ThrowsError (Type, TypeEnv)
evalType ty env = case ty of
    
    TyInt -> return (TyInt, env)
    TyBool -> return (TyBool, env)
    TyUnit -> return (TyUnit, env)

    -- Fix ty vars...
    (TyVar name) -> if length name < 1
        then do
            v <- lookupEnv name env
            return (v, env)
        else return (TyVar name, env)

    (TyAbs param body) -> return (TyAbs param body, env)

    (TyApp fun arg) -> do
        ((TyAbs (TyVar x) body), clo) <- evalType fun env
        (argv, _) <- evalType arg env
        let env' = Map.insert x argv clo
        evalType body env'

    (TyTaggedUnion branches) -> do
        newBraches <- mapM (\(name, tys) -> do
            tys' <- mapM et tys
            return (name, tys')) branches
        return (TyTaggedUnion newBraches, env)
        where
            et ty = do
                (ty', _) <- evalType ty env
                return ty'

type KindConstr = (Kind, Kind)

type KindUnifier = (KSubst, [KindConstr])

type KindSolve a = ExceptT LangErr Identity a

newtype KSubst = KSubst (Map.Map Kind Kind)
    deriving (Eq, Ord, Show, Monoid)

class KSubstitutable a where
    kapply :: KSubst -> a -> a

instance KSubstitutable Kind where
    kapply _ KStar = KStar
    kapply (KSubst s) t@(KVar a) = Map.findWithDefault t t s
    kapply s (KArr k1 k2) = KArr (kapply s k1) (kapply s k2)

instance KSubstitutable a => KSubstitutable [a] where
    kapply = map . kapply

instance KSubstitutable KindConstr where
    kapply s (k1, k2) = (kapply s k1, kapply s k2)

runKSolve :: [KindConstr] -> Either LangErr KSubst
runKSolve cs = runIdentity $ runExceptT $ kSolver st
    where st = (mempty, cs)

kSolver :: KindUnifier -> KindSolve KSubst
kSolver (su, cs) = case cs of
    [] -> return su
    ((k1, k2) : cs0) -> do
        su1 <- kindUnifies k1 k2
        kSolver (su1 `kcompose` su, (kapply su1 cs0))

kcompose :: KSubst -> KSubst -> KSubst
kcompose (KSubst s1) (KSubst s2) = KSubst $ Map.map (kapply (KSubst s1)) s2 `Map.union` s1

kindUnifyMany :: [Kind] -> [Kind] -> KindSolve KSubst
kindUnifyMany [] [] = return mempty
kindUnifyMany (k1 : ks1) (k2 : ks2) = do
    su1 <- kindUnifies k1 k2
    su2 <- kindUnifyMany (kapply su1 ks1) (kapply su1 ks2)
    return (su2 `kcompose` su1)
kindUnifyMany k1 k2 = error "kind unification failed"

kindUnifies :: Kind -> Kind -> KindSolve KSubst
kindUnifies k1 k2 | k1 == k2 = return mempty
kindUnifies k@(KVar name) t = k `kBind` t
kindUnifies t k@(KVar name) = k `kBind` t
kindUnifies (KArr k1 k2) (KArr k3 k4) = kindUnifyMany [k1, k2] [k3, k4]
kindUnifies k1 k2 = error "kind inference failed"

kBind :: Kind -> Kind -> KindSolve KSubst
kBind a t
    | t == a = return mempty
    | otherwise = return $ (KSubst $ Map.singleton a t)

fresh :: Infer Type Type
fresh = do
    s <- get
    put s { nameIndex = nameIndex s + 1 }
    return $ TyVar (names !! nameIndex s)

instantiate :: Type -> Infer Type Type
instantiate (Forall xs ty) = do
    names <- mapM (\_ -> fresh) xs
    let s = Subst $ Map.fromList (zip xs names)
    return $ apply s ty

generalize :: TypeEnv -> Type -> Type
generalize env t = Forall xs t
    where xs = Set.toList $ freeTyVars t `Set.difference` freeTyVars env

lookupVar :: String -> TypeEnv -> Infer Type Type
lookupVar name env = do
    sc <- lift $ lookupEnv name env
    t <- instantiate sc
    return t

lookupEnv :: String -> Map.Map String a -> ThrowsError a
lookupEnv name env = case Map.lookup name env of
    Just sc -> return sc
    Nothing -> throwError $ ErrTyVarNotFound name

solveConstrs :: [Constraint] -> Type -> Infer Type Type
solveConstrs constrs ty = case runSolve constrs  of
    Left err -> throwError err
    Right s -> return $ apply s ty

runMkConstr :: Expr -> ThrowsError ((Type, TypeEnv), (InferState Type))
runMkConstr ex = runStateT (mkConstrs ex Map.empty) initInfer

addConstr :: (a, a) -> Infer () a
addConstr c = do
    is <- get
    put is { constraints = c : (constraints is) }

getConstrs :: Infer [(a, a)] a
getConstrs = fmap constraints get

mkConstrs :: Expr -> TypeEnv -> Infer (Type, TypeEnv) Type
mkConstrs expr env = case expr of

    (EVar _ name) -> do
        instanciated <- lookupVar name env
        return (instanciated, env)

    (ELit _ (LInt _)) -> return (TyInt, env)

    (ELit _ (LBool _)) -> return (TyBool, env)

    (EUnit _) -> do
        error $ show env

    (EDef _ name args body) -> do
        argTys <- mapM (\_ -> fresh) args
        t2 <- fresh
        let tf = (TyFunc argTys t2)
        let env' = Map.insert name (Forall [] tf) env
        let env'' = Map.union env' $ Map.unions $ zipWith (\n ty -> Map.insert n (Forall [] ty) env') args argTys
        (tyBody, _) <- mkConstrs body env''
        cs <- getConstrs
        funcTy <- solveConstrs cs (TyFunc argTys tyBody)
        let funcTy' = generalize env funcTy
        let env''' = Map.insert name funcTy' env
        return ((TyFunc argTys tyBody), env''')

    (EInvoke _ name argExprs) -> do
        argResults <- mapM (\ex -> mkConstrs ex env) argExprs
        let argExprTys = map fst argResults
        func <- lookupVar name env
        newVar <- fresh
        addConstr (func, TyFunc argExprTys newVar)
        return (newVar, env)

    (ESeq t1 t2) -> do
        (tyT1, env') <- mkConstrs t1 env 
        mkConstrs t2 env'

    -- generalise to all binary operations....
    (EBinexp _ op t1 t2) -> do
        (tyT1, env') <- mkConstrs t1 env 
        (tyT2, env'') <- mkConstrs t2 env'
        mapM_ addConstr $ [(tyT2, TyInt), (tyT1, TyInt)]
        return (TyInt, env'')

    (ETag _ name exprs) -> do
        tyTagged <- lift $ lookupEnv name env
        tyExprs <- mapM (\e -> mkConstrs e env) exprs
        -- evaluate the types here, and return the result with the actual type
        -- substituted, as much as possible
        -- Fix this. Folding an application is not the right answer here
        let app = foldl TyApp tyTagged (map fst tyExprs)
        (evaled, e) <- lift $ evalType app env
        let evaled' = apply (Subst $ Map.mapKeys TyVar e) evaled
        return (getBody evaled', env)

    (ETyDef name params body) -> do
        let tyabs = foldr TyAbs body params
        let env' = Map.insert name tyabs env
        case body of
            (TyTaggedUnion fields) -> do
                let env'' = Map.union env' $ Map.unions $ map (\(n, _) -> Map.insert n tyabs env) fields
                return (tyabs, env'')
            _ -> return (tyabs, env')

    (ECase _ expr branches) -> do
        (tyExpr, env') <- mkConstrs expr env
        let (tag1, ex1) = head branches
        case tag1 of
            (ETag _ name exprs) -> do
                newVars <- mapM (\_ -> fresh) exprs
                let env'' = Map.union env' $ Map.fromList $ zip (map (\(EVar _ name) -> name) exprs) (map (Forall []) newVars)
                -- can't evaluate braches as tags, cause they don't unify...
                -- Fixing the evaluation of Tag would fix this
                (tyBranches, _) <- mkConstrs tag1 env''
                addConstr (tyExpr, tyBranches)
                (tyEx, env''') <- mkConstrs ex1 env''
                -- need unify all branches!
                -- error $ show exprConstr
                return (tyEx, env''')

-- recon :: Expr -> Infer Type
-- recon expr = case expr of
--     (EVar _ name) -> lookupEnv name
-- 
--     (ETag _ name exprs) -> do
--         tyTagged <- lookupEnv name
--         tysExprs <- mapM recon exprs
--         case tyTagged of
--             (TyTaggedUnion tags) -> do
--                 case lookup name tags of
--                     (Just types) -> tell $ zip types tysExprs
--                     Nothing -> throwError $ ErrTyVarNotFound name (Map.empty)
--         return tyTagged
-- 
--     (ELit _ (LInt _)) -> return TyInt
-- 
--     (ELit _ (LBool _)) -> return TyBool
-- 
--     (ELit _ (LString _)) -> return TyString
-- 
--     (EUnit _) -> do
--         e <- fmap env get
--         error $ show e
-- 
--     (ETaggedUnion _ _ _) -> return TyUnit
-- 
--     (EDef _ name args body) -> do
--         argTys <- mapM (\_ -> fresh) args
--         t2 <- fresh
--         let tf = (TyFunc argTys t2)
--         inEnv name (Forall [] tf)
--         zipWithM (\n ty -> inEnv n $ Forall [] ty) args argTys
--         tyBody <- recon body
--         mapM_ outEnv args
--         outEnv name
--         e <- fmap env get
--         tell $ [(t2, tyBody)]
--         tell $ [(tyBody, t2)]
--         -- error $ show $ 
--         let funcTy' = generalize e $ TyFunc argTys tyBody -- Should be tf?
--         -- let funcTy' = Forall [TyVar "a1"] $ TyFunc argTys tyBody -- Should be tf?
--         inEnv name funcTy'
--         return $ TyFunc argTys tyBody
-- 
--     (EInvoke _ name argExprs) -> do
--         argExprTys <- mapM recon argExprs
--         func <- lookupEnv name
--         newVar <- fresh
--         e <- fmap env get
--         tell $ [(func, TyFunc argExprTys newVar)]
--         -- If invoke is called and lookup returns a var, then that var must not
--         -- be instanciated. Instead its type is monomorphic
--         -- must be rank one. etc etc
--         case runSolve [(func, TyFunc argExprTys newVar)] of
--             Left err -> throwError err
--             Right s -> return $ apply s newVar
--         --return newVar
-- 
--     (ESeq first second) -> do
--         recon first
--         recon second
-- 
--     (EAssign _ name expr) -> do
--         tyExpr <- recon expr
--         e <- fmap env get
--         inEnv name (generalize e tyExpr)
--         return TyUnit
-- 
--     (ECallShell _ command args) -> do
--         commandTy <- recon command
--         argsTy <- recon args
--         tell $ [(commandTy, TyString), (argsTy, TyString)]
--         return TyString
-- 
--     (EPrint _ arg) -> do
--         argTy <- recon arg
--         tell $ [(argTy, TyString)]
--         return TyUnit
-- 
--     (EBinexp info op left right) -> do
--         e <- fmap env get
--         tyLeft <- recon left
--         tyRight <- recon right
--         tell $ [(tyLeft, TyInt), (tyRight, TyInt)]
--         return $ opTypes op
-- 
--     (EIf _ cond tr fl) -> do
--         tyCond <- recon cond
--         tyTr <- recon tr
--         tyFl <- recon fl
--         tell $ [(tyCond, TyBool), (tyTr, tyFl)]
--         return tyFl
-- 
--     (EVector info elems) -> if V.null elems then return $ TyVec TyUnit else do
--         tyVec <- V.mapM recon elems
--         tell $ V.toList $ V.map (\t -> (V.head tyVec, t)) (V.tail tyVec)
--         return $ TyVec (V.head tyVec)
-- 
--     (EIndex info name idx) -> do
--         vecExpr <- lookupEnv name
--         tyIndex <- recon idx
--         tell $ [(tyIndex, TyInt)]
--         case vecExpr of
--             (TyVec elemTy) -> return elemTy
--             (TyVar varName) -> do
--                 freshName <- fresh
--                 tell $ [(vecExpr, TyVec freshName)]
--                 return freshName
-- 
--     (EFor info iter seq body) -> do
--         tySeq <- recon seq
--         case tySeq of
--             (TyVec elemTy) -> doBody iter elemTy
--             (TyVar name) -> do
--                 freshTy <- fresh
--                 tell $ [(tySeq, TyVec freshTy)]
--                 doBody iter freshTy
--         where 
--             doBody (EVar _ name) ty = do
--                 tyIter <- fresh
--                 tell $ [(tyIter, ty)]
--                 insertIntoEnv name ty
--                 recon body
--                 return TyUnit
-- 
--    (ECase info expr branches) -> do
--        tyExpr <- recon expr
--        union <- tagsToUnionsTy branches
--        TyTaggedUnion fts <- getFts $ fst $ head branches
--        tell $ [(tyExpr, union)]
--        branchTys branches fts
--        where getFts (ETag _ name _) = lookupEnv name

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
--     tell $ map (\ty -> (tyT1, ty)) restTys
--     return tyT1
--     where
--         tyLookup ((ETag _ name args), result) = do
--             tys <- lift $ fieldLookup name fts
--             zipWithM (\(EVar _ n) ty -> insertIntoEnv n ty) args tys
--             recon result
-- 
-- fieldLookup :: String -> [(String, [Type])] -> ThrowsError [Type]
-- fieldLookup name tys = case lookup name tys of
--     Just ty -> return ty
--     Nothing -> throwError $ ErrNotInVariantFields name
