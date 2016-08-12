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
-- 
-- data NameState = NameState {
--       count :: Int
--     , env :: TypeEnv
--     }
--                             
-- type Infer a = (RWST TypeEnv [Constraint] NameState ThrowsError a)
-- 
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

-- instance Substitutable Type where

instance Substitutable a => Substitutable [a] where
    apply = map . apply
    freeTyVars = foldr (Set.union . freeTyVars) Set.empty

instance Substitutable (Map.Map String Type) where
    apply s env = Map.map (apply s) env
    freeTyVars env = freeTyVars $ Map.elems env

instance Substitutable Constraint where
    apply s (t1, t2) = (apply s t1, apply s t2)
    freeTyVars (t1, t2) = freeTyVars t1 `Set.union` freeTyVars t2

-- 
-- dataToEnv :: TypeEnv -> Expr -> TypeEnv
-- dataToEnv env expr = case expr of
--     (ETaggedUnion _ name ty) -> Map.union (Map.insert name ty env) $ case ty of
--         (TyTaggedUnion constrs) -> Map.unions $ fmap (\x -> Map.insert x ty env) $ fmap fst constrs
--         (TyRec name (TyTaggedUnion constrs)) -> Map.unions $ fmap (\x -> Map.insert x ty env) $ fmap fst constrs
--         _ -> Map.empty
--     (ESeq first second) -> Map.union (dte first) (dte second)
--         where dte = dataToEnv env
--     _ -> env
-- 
names :: [String]
names = zipWith (\c n -> c ++ (show n)) (repeat "a") (iterate (+1) 1)

-- fresh :: Infer Type
-- fresh = do
--     s <- get
--     put s { count = count s + 1 }
--     return $ TyVar (names !! count s)
-- 
-- initNameState ::TypeEnv -> NameState
-- initNameState e = NameState { count = 0, env = e }
-- 
-- runInfer :: TypeEnv -> Infer Type -> ThrowsError (Type, [Constraint])
-- runInfer env m = evalRWST m env (initNameState env)
-- 
-- inferType :: Expr -> ThrowsError (Type, [Constraint])
-- inferType expr = runInfer Map.empty (recon expr)
--     --where dataEnv = dataToEnv Map.empty expr
-- 
-- infer :: Expr -> ThrowsError Scheme
-- infer ex = case inferExpr Map.empty ex of
--     Left err -> throwError err
--     Right ty -> return ty
-- 
constraintsExpr :: Expr -> ThrowsError ([Constraint], Subst, Type, Type)
constraintsExpr ex = do
    (ty, _, constrs) <- runMkConstr ex
    -- (ty, constrs) <- runInfer Map.empty (recon ex)
    case runSolve constrs of
        Left err -> throwError err
        Right subst -> return $ (constrs, subst, ty, (closeOver $ apply subst ty))
-- 
-- inferExpr :: TypeEnv -> Expr -> Either LangErr Scheme
-- inferExpr env ex = case runExcept $ runInfer env (recon ex) of
--     Left err -> Left err
--     Right (ty, constrs) -> case runSolve constrs of
--         Left err -> Left err
--         Right subst -> Right $ closeOver $ apply subst ty
-- 
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
        normtype (TyVar a) = case lookup a ord of
            Just x -> TyVar a
            Nothing -> error "ty var not in sig"
        normtype TyInt = TyInt
        normtype TyBool = TyBool
        normtype TyUnit = TyUnit
        normtype (TyTaggedUnion fields) = TyTaggedUnion fields
        normtype (TyAbs param body) = body
        normtype err = error $ show err

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

-- Testing for type equivalence
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
unifies t1 t2 = throwError $ ErrUnify [t1] [t2]
-- unifies t1 t2 = throwError $ ErrUnify [t1] [t2]

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

-- Expand a type to check internal structure
expand :: Type -> Type
expand (TyApp (TyAbs param body) ty) = expand (apply newSubst body)
    where newSubst = Subst $ Map.singleton param ty
expand ty = ty
-- 
-- instantiate :: Scheme -> Infer Type
-- instantiate (Forall xs ty) = do
--     names <- mapM (\_ -> fresh) xs
--     let s = Subst $ Map.fromList (zip xs names)
--     return $ apply s ty
-- 
generalize :: TypeEnv -> Type -> Type
generalize env t = Forall xs t
    where xs = Set.toList $ freeTyVars t `Set.difference` freeTyVars env `Set.difference` (Set.fromList [])
    -- where xs = error $ show $ freeTyVars t `Set.difference` freeTyVars env
-- 
-- insertIntoEnv :: String -> Scheme -> Infer ()
-- insertIntoEnv name ty = do
--     s <- get
--     let newEnv = Map.insert name ty (env s)
--     put s { env = newEnv }
--     return ()
-- 
-- lookupEnv :: String -> Infer Type
-- lookupEnv name = do
--     e <- fmap env get
--     case Map.lookup name e of
--         Just sc -> do
--             t <- instantiate sc
--             return t
--         Nothing -> throwError $ ErrTyVarNotFound name e
-- 
-- inEnv :: String -> Scheme -> Infer ()
-- inEnv name scheme = do
--     s <- get
--     let oldEnv = env s
--     let newEnv = Map.delete name oldEnv
--     insertIntoEnv name scheme
-- 
-- outEnv :: String -> Infer ()
-- outEnv name = do
--     s <- get
--     let oldEnv = env s
--     let newEnv = Map.delete name oldEnv
--     put s { env = newEnv }
-- 
fresh' :: Constr Type
fresh' = do
    s <- get
    put s { index = index s + 1 }
    return $ TyVar (names !! index s)

instantiate' :: Type -> Constr Type
instantiate' (Forall xs ty) = do
    names <- mapM (\_ -> fresh') xs
    let s = Subst $ Map.fromList (zip xs names)
    return $ apply s ty

lookupVar :: String -> TypeEnv -> Constr Type
lookupVar name env = do
    sc <- lift $ lookupEnv name env
    t <- instantiate' sc
    return t

lookupEnv :: String -> TypeEnv -> ThrowsError Type
lookupEnv name env = case Map.lookup name env of
    Just sc -> return sc
    Nothing -> throwError $ ErrTyVarNotFound name env

solveConstrs :: [Constraint] -> Type -> Constr Type
solveConstrs constrs ty = case runSolve constrs  of
    Left err -> throwError err
    Right s -> return $ apply s ty

data NamesSupply = NamesSupply {
    index :: Int
}

initSupply :: NamesSupply
initSupply = NamesSupply { index = 0 }

type Constr a = StateT NamesSupply ThrowsError a 

runMkConstr :: Expr -> ThrowsError (Type, TypeEnv, [Constraint])
runMkConstr ex = evalStateT (mkConstrs ex Map.empty []) initSupply

getFirst3 :: (a, b, c) -> a
getFirst3 (a, b, c) = a

getThird3 :: (a, b, c) -> c
getThird3 (a, b, c) = c

mkConstrs :: Expr -> TypeEnv -> [Constraint] -> Constr (Type, TypeEnv, [Constraint])
mkConstrs expr env constrs = case expr of

    (EVar _ name) -> do
        instanciated <- lookupVar name env
        return (instanciated, env, constrs)

    (ELit _ (LInt _)) -> return (TyInt, env, constrs)

    (ELit _ (LBool _)) -> return (TyBool, env, constrs)

    (EUnit _) -> do
        error $ show env

    (EDef _ name args body) -> do
        argTys <- mapM (\_ -> fresh') args
        t2 <- fresh'
        let tf = (TyFunc argTys t2)
        let env' = Map.insert name (Forall [] tf) env
        let env'' = Map.unions $ zipWith (\n ty -> Map.insert n (Forall [] ty) env') args argTys
        (tyBody, _, bodyContrs) <- mkConstrs body env'' constrs
        funcTy <- solveConstrs (bodyContrs ++ constrs) (TyFunc argTys tyBody)
        let funcTy' = generalize env funcTy
        let env''' = Map.insert name funcTy' env
        return ((TyFunc argTys tyBody), env''', bodyContrs ++ constrs)

    (EInvoke _ name argExprs) -> do
        argResults <- mapM (\ex -> mkConstrs ex env constrs) argExprs
        let argExprTys = map getFirst3 argResults
        let argConstrs = concatMap getThird3 argResults
        func <- lookupVar name env
        newVar <- fresh'
        return (newVar, env, (func, TyFunc argExprTys newVar) : constrs ++ argConstrs)

    (ESeq t1 t2) -> do
        (tyT1, env', constrs1) <- mkConstrs t1 env constrs
        mkConstrs t2 env' constrs1

    (ETag _ name exprs) -> do
        tyTagged <- lift $ lookupEnv name env
        tyExprs <- mapM (\e -> mkConstrs e env constrs) exprs
        let app = foldl TyApp tyTagged (map getFirst3 tyExprs)
        return (app, env, constrs)

    (ETyDef name params body) -> do
        let tyabs = foldr TyAbs body params
        let env' = Map.insert name tyabs env
        case body of
            (TyTaggedUnion fields) -> do
                let env'' = Map.unions $ map (\(n, _) -> Map.insert n tyabs env) fields
                return (TyUnit, Map.union env' env'', constrs)
            _ -> return (TyUnit, env', constrs)

transTy :: TypeEnv -> Type -> ThrowsError Type
transTy tyEnv ty = case ty of
    
    (TyVar name) -> do
        ty <- lookupEnv name tyEnv
        return ty
    

-- 
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
