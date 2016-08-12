module Main where

import Control.Monad.Except

import System.Environment

import qualified Data.Map.Strict as Map

import Syntax
import Parser
import Eval
import Type

printEval :: Expr -> IO ()
printEval inp = do
    evaled <- evalExpr inp
    case evaled of
        (Right expr) -> return ()
        (Left err) -> return ()

process :: String -> ThrowsError ([Constraint], Subst, Type, Type)
process inp = do
    parsed <- parseTopLevel inp
    constraintsExpr parsed
    -- evalExpr parsed

printConstr :: ([Constraint], Subst, Type, Type) -> IO ()
printConstr (cs, (Subst s), ty, sc) = do
    putStrLn "Constraints:"
    mapM_ (putStrLn . show) cs
    putStrLn "Subst:"
    mapM_ (putStrLn . show) (Map.assocs s)
    putStrLn "Type:"
    putStrLn (show ty)
    putStrLn "Scheme:"
    putStrLn (show sc)

foo inp = case runExcept $ process inp of
    Right val -> putStrLn (show val)
    Left err -> putStrLn (show err)

main = do
    path <- fmap head getArgs 
    inp <- readFile path
    case runExcept $ process inp of
        Right val -> printConstr val
        Left err -> (ppErr err)

ppList :: Show a => [a] -> IO ()
ppList li = mapM_ (putStrLn . show) li

ppErr :: LangErr -> IO ()
ppErr (ErrTyVarNotFound var env) = do
    putStrLn $ "Can't find type variable: " ++ var ++ ", env: "
    ppList $ Map.assocs env
ppErr err = putStrLn $ show err
