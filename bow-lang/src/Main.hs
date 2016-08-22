module Main where

import Control.Monad.Except

import Data.List
import qualified Data.Map.Strict as Map

import System.Environment

import Syntax
import Parser
import Eval
import Type

--runType :: String -> ThrowsError 
runType inp = do
    parsed <- parseTopLevel inp
    constraintsExpr parsed

process :: Bool -> String -> (String -> ThrowsError ([(Type, Type)], Subst, Type, Type)) -> IO ()
process isVerbose path func = do
    source <- readFile path
    case runExcept $ func source of
        Left err -> putStrLn $ show err
        Right (cs, sbs, pre, post) -> putStrLn $ if isVerbose 
            then pretty (cs, sbs, pre, post) 
            else show post

pretty (cs, Subst sbs, pre, post) = "Constraints:\n" ++ csStr ++ "\nSubsts:\n" ++ sbsStr ++ "\nType:\n" ++ (show post)
    where
        csStr = intercalate "\n" $ map show cs
        sbsStr = intercalate "\n" $ map show $ Map.toList sbs

processEval :: String -> IO ()
processEval inp = undefined

main = do
    args <- getArgs
    case args of
        -- [] -> repl
        ["--verbose", "--type", path] -> process True path runType
        ["--type", path] -> process False path runType
        ["--eval", path] -> process False path runType
        other -> putStrLn $ "Unknown option: " ++ show other
