module Type where
import Syntax

import qualified Data.Map.Strict as Map

import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Except

type TypeEnv = Map.Map String Type

type VarEnv = Map.Map String Type

data Names = Names {
    count :: Int
    , tEnv :: TypeEnv
    , vEnv :: VarEnv
    }

type Infer a = RWST TypeEnv [Constraint] Names (Except LangErr) a

type Constraint = (Type, Type)

runInfer ex = evalRWST (typeCheck ex) emp initNameState
    where emp = Map.empty

runSolve constrs = evalRWST (unify constrs) emp initNameState
    where emp = Map.empty

inferExpr :: Expr -> ThrowsError (Type, [Constraint])
inferExpr ex = do
    (ty, cs) <- runInfer ex
    (cs', _) <- runSolve cs
    return (ty, cs')

fresh :: Infer String
fresh = do
    s <- get
    put s { count = count s + 1 }
    return (names !! count s)

initNameState :: Names
initNameState = Names { count = 0, tEnv = Map.empty, vEnv = Map.empty }

names :: [String]
names = zipWith (\c n -> c ++ (show n)) (repeat "a") (iterate (+1) 1)

substType :: Type -> TypeEnv -> Infer Type
substType ty env = case ty of
    (TyVar name) -> return $ Map.findWithDefault ty name env

    (TyApp tycon args) -> do
        substed <- mapM (\t -> substType t env) args
        return $ TyApp tycon substed

    (TyPoly names ty) -> do
        newNames <- mapM (\_ -> fresh) names
        ty' <- substType ty (Map.fromList $ zipWith (\old new -> (old, TyVar new)) names newNames)
        newTy <- substType ty' env
        return $ TyPoly newNames newTy

unify :: [Constraint] -> Infer [Constraint]
unify [] = return []
unify ((TyApp tycon1 tyTs, TyApp tycon2 tySs) : rest) = do
    when (tycon1 /= tycon2) (throwError $ ErrDefault "can't unify")
    unify $ (zip tyTs tySs) ++ rest
unify ((TyPoly names1 ty1, TyPoly names2 ty2) : rest) = do
    c2 <- substType ty2 (Map.fromList $ zipWith (\old new -> (old, TyVar new)) names1 names2)
    unify $ (ty1, c2) : rest
unify ((TyVar name1, TyVar name2) : rest) = do
    when (name1 /= name2) (throwError $ ErrDefault "name mismatch")
    unify rest
unify constrs = throwError $ ErrDefault $ "can't unify" ++ (show constrs)

expand :: Type -> Infer Type
expand u = return u

lookupEnv :: String -> Infer Type
lookupEnv name = do
    env <- fmap vEnv get
    case Map.lookup name env of
        Nothing -> throwError $ ErrDefault "can't find var"
        Just v -> return v

intoTEnv :: String -> Type -> Infer ()
intoTEnv name ty = do
    s <- get
    let newEnv = Map.insert name ty (tEnv s)
    put s {tEnv = newEnv}

intoVEnv :: String -> Type -> Infer ()
intoVEnv name ty = do
    s <- get
    let newEnv = Map.insert name ty (vEnv s)
    put s {vEnv = newEnv}

typeCheck :: Expr -> Infer Type
typeCheck ex = case ex of
    (EInt _) -> return $ TyApp TyInt []

    (EBool _) -> return $ TyApp TyBool []

    (FunDecl name [z] [(x, t1)] t2 body) -> do
        beta <- fresh
        intoTEnv z (TyVar beta)
        intoVEnv name (TyPoly [beta] (TyApp TyArrow [t1, t2]))
        intoVEnv x t1
        t3 <- typeCheck body
        tell $ [(t2, t3)]
        return (TyApp TyArrow [t1, t2])

    (EVar name) -> lookupEnv name 

    (EInvoke e1 [ty] [e2]) -> do
        tf <- typeCheck e1
        te <- typeCheck e2
        tf' <- expand tf
        case tf' of
            (TyPoly [beta] (TyApp TyArrow [t1, t2])) -> do
                st1 <- substType t1 (Map.singleton beta ty)
                tell $ [(te, st1)]
                substType t2 (Map.singleton beta ty)

    (EBinop _ e1 e2) -> do
        t1 <- typeCheck e1
        t2 <- typeCheck e2
        tell $ [(t1, TyApp TyInt []), (t2, TyApp TyInt [])]
        return $ TyApp TyInt []

    (ESeq e1 e2) -> do
        typeCheck e1
        typeCheck e2

    _ -> error $ show ex

