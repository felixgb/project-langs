module Main where

import System.Environment

import Parser
import Type
import Eval

main :: IO ()
main = do
    filePath <- fmap head getArgs
    inp <- readFile filePath
    putStrLn $ show $ process inp

-- How to do this without nested eithers?
process :: String -> Either String (Either String Value)
process inp = do
    parsed <- parsecErrToString inp
    checked <- checkType parsed 
    return $ runEval parsed

parseAndEval inp = do
    parsed <- parsecErrToString inp
    return $ runEval parsed

parsecErrToString inp = case parseExp inp of
    Right ast -> Right ast
    Left parsecErr -> Left $ show parsecErr
