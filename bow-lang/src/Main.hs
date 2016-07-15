module Main where

import Control.Monad.Except

import System.Environment

import qualified Data.Map.Strict as Map

import Syntax
import Parser
import Eval
import Type

process :: String -> ThrowsError Type
process inp = do
    parsed <- parseTopLevel inp
    infer parsed
    -- evalExpr parsed

foo inp = case runExcept $ process inp of
    Right val -> putStrLn (show val)
    Left err -> putStrLn (show err)

main = do
    path <- fmap head getArgs 
    inp <- readFile path
    case runExcept $ process inp of
        Right val -> putStrLn (show val)
        Left err -> (ppErr err)

ppList :: Show a => [a] -> IO ()
ppList li = mapM_ (putStrLn . show) li

ppErr :: LangErr -> IO ()
ppErr (ErrTyVarNotFound var env) = do
    putStrLn $ "Can't find type variable: " ++ var ++ ", env: "
    ppList $ Map.assocs env
ppErr err = putStrLn $ show err

