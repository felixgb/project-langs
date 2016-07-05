module Top where

import System.Environment
import Control.Monad.Except

import Syntax
import Parser
import Type
import Eval
-- 
-- run :: IO ()
-- run = do
--     filePath <- fmap head getArgs
--     inp <- readFile filePath
--     putStrLn $ show $ process inp
-- 

process :: String -> ThrowsError Value
process inp = do
    parsed <- parseExp inp
    infer parsed 
    runEval parsed

-- parseAndEval inp = do
--     parsed <- parsecErrToString inp
--     return $ runEval parsed
-- 
-- parsecErrToString inp = case parseExp inp of
--     Right ast -> Right ast
--     Left parsecErr -> Left $ show parsecErr
