module Main where

import Control.Monad.Except

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

main = undefined
