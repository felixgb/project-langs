module Main where

import Parser
import Codegen
import Emit


import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.General.AST as AST

initModule :: AST.Module
initModule = emptyModule "jit?"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
    let res = parseTopLevel source
    case res of
        Left err -> print err >> return Nothing
        Right ex -> do
            ast <- codegen modo ex
            return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fname] -> processFile fname >> return ()
