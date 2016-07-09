module Top where

import System.Environment
import Control.Monad.Except

import Syntax
import Parser
import Type
import Eval
-- 
-- main = do
--     path <- fmap head getArgs
--     code <- readFile path
--     putStrLn $ show $ process code
-- 
process :: String -> ThrowsError Value
process inp = do
    parsed <- parseExp inp
    -- infer parsed 
    runEval parsed
