module Eval where

import Control.Monad.Except
import Control.Monad.State

import System.Process

import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Map.Strict as Map

import Syntax

type Eval a = ExceptT LangErr (StateT VarEnv IO) a


evalExpr :: Expr -> IO (Either LangErr Expr)
evalExpr expr = runEval emptyEnv (eval expr)
    where emptyEnv = Map.empty

runEval :: VarEnv -> Eval Expr -> IO (Either LangErr Expr)
runEval env m = evalStateT (runExceptT m) env

lookupVar :: String -> Eval Expr
lookupVar x = do
    env <- get
    case Map.lookup x env of
        Nothing -> throwError $ ErrVarNotFound x
        Just val -> (simplify val) >>= (\x -> return x)

simplify :: Expr -> Eval Expr
simplify (EVar info name) = lookupVar name
simplify ex = return ex

insertIntoEnv :: String -> Expr -> Eval ()
insertIntoEnv name expr = do
    env <- get
    put $ Map.insert name expr env

eval :: Expr -> Eval Expr
eval exp = case exp of
    (EVar info name) -> lookupVar name

    ex@(ELit info lit) -> return ex

    (ESeq first second) -> do
        eval first
        eval second

    ex@(EUnit info) -> return ex

    (ETaggedUnion _ _ _) -> return $ EUnit DummyInfo

    (EAssign info name expr) -> do
        expr' <- eval expr
        insertIntoEnv name expr'
        return $ EUnit DummyInfo

    (EDef info name args body) -> do
        let scope = Map.empty
        insertIntoEnv name (EFunction name args body scope)
        return $ EUnit DummyInfo

    (EInvoke info name args) -> do
        evaledArgs <- mapM eval args
        func <- lookupVar name
        case func of
            (EFunction _ argNames body scope) -> do
                mapM_ eval $ zipWith (EAssign DummyInfo) argNames args
                eval body

    (EBinexp info op left right) -> do
        left'  <- eval left
        right' <- eval right
        return $ lookupBinop op left' right'

    (EIf info cond e1 e2) -> do
        cond' <- eval cond
        case cond' of
            (ELit info (LBool True)) -> eval e1
            (ELit info (LBool False)) -> eval e2

    (ECallShell info (ELit _ (LString commandStr)) (ELit _ (LString argsStr))) -> do
        out <- liftIO $ readProcess commandStr (words argsStr) ""
        return (ELit DummyInfo (LString out))

    (EPrint info (ELit _ (LString inp))) -> do
        liftIO $ putStr inp
        return $ EUnit DummyInfo

    (EFor info (EVar inf name) range body) -> do
        (EVector _ vec) <- simplify range
        forM_ (V.toList vec) $ \i -> do
            i' <- eval i
            insertIntoEnv name i'
            eval body
        return $ EUnit DummyInfo
    
    (ECase info tag branches) -> do
        t@(ETag _ name exs) <- simplify tag
        case L.find (\((ETag _ tName _), _) -> tName == name) branches of
            Just (ETag _ _ vars, expr) -> do
                exprs' <- mapM eval exs
                zipWithM insertVar vars exprs'
                eval expr
            Nothing -> throwError $ ErrVarNotFound name
            where insertVar (EVar _ name) ex = insertIntoEnv name ex

    t@(ETag info name exs) -> return t

    v@(EVector info exs) -> return v

    (EIndex info name vecIdx) -> do
        (EVector _ exs) <- lookupVar name
        (ELit _ (LInt n)) <- eval vecIdx
        case exs V.!? n of
            Just e -> return e
            Nothing -> throwError $ ErrIndexOutOfBounds n name

    err -> error (show err)
        
lookupBinop :: Op -> Expr -> Expr -> Expr
lookupBinop Plus (ELit _ (LInt l)) (ELit _ (LInt r)) = ELit DummyInfo $ LInt (l + r)
