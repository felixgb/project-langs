module Main where

import Control.Monad.Except

import System.Environment

import qualified Data.Map.Strict as Map

import Syntax
import Parser
import Eval
import Type

parseAndInfer :: String -> ThrowsError Expr
parseAndInfer inp = do
    parsed <- parseTopLevel inp
    infer parsed
    return parsed

printInfer :: String -> IO ()
printInfer inp = do
    case runExcept $ parseTopLevel inp of
        Right val -> do
            case runExcept $ infer val of
                Right x -> putStrLn (show x)
                Left err -> ppErr err
        Left err -> print err

printEval :: Expr -> IO ()
printEval inp = do
    evaled <- evalExpr inp
    case evaled of
        (Right expr) -> return ()
        (Left err) -> return ()

main = do
    path <- fmap head getArgs 
    inp <- readFile path
    printInfer inp
    -- case runExcept $ parseAndInfer inp of
    --     Right val -> printEval val
    --     Left err -> (ppErr err)

ppList :: Show a => [a] -> IO ()
ppList li = mapM_ (putStrLn . show) li

ppErr :: LangErr -> IO ()
ppErr (ErrTyVarNotFound var env) = do
    putStrLn $ "Can't find type variable: " ++ var ++ ", env: "
    ppList $ Map.assocs env
ppErr (ErrUnifyUnsolvable tys) = do
    putStrLn $ "Cannot unify: "
    ppList tys
ppErr err = putStrLn $ show err

