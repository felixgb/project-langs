{-# LANGUAGE OverloadedStrings #-}

module Emit where

import Codegen
import qualified Syntax as S

import LLVM.General.Module
import LLVM.General.Context

import LLVM.General.AST.Constant as C
import LLVM.General.AST.Float as F
import LLVM.General.AST.FloatingPointPredicate as FP

import qualified LLVM.General.AST as AST

import Control.Monad.Except
import Control.Applicative

import Data.Word
import Data.Int
import qualified Data.Map as Map

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
    define double name fnagrs bls
    where
        fnagrs = toSig args
        bls = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            forM args $ \a -> do
                var <- alloca double
                store var (local $ AST.Name a)
                assign a var
            cgen body >>= ret
codegenTop (S.Extern name args) = do
    external double name fnargs
    where fnargs = toSig args
codegenTop exp = do
    define double "main" [] blks
    where
        blks = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            cgen exp >>= ret

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.UnaryOp op a) = do
    cgen $ S.Call ("unary" ++ op) [a]
cgen (S.BinaryOp "=" (S.Var var) val) = do
    a <- getvar var
    cval <- cgen val
    store a cval
    return cval
cgen (S.BinaryOp op a b) = do
    case Map.lookup op binops of
        Just f -> do
            ca <- cgen a
            cb <- cgen b
            f ca cb
        Nothing -> error "No such operator"
cgen (S.Var x) = getvar x >>= load
cgen (S.Float n) = return $ constant $ C.Float (F.Double n)
cgen (S.Call fn args) = do
    largs <- mapM cgen args
    call (externf $ AST.Name fn) largs

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
    test <- fcmp FP.ULT a b
    uitofp double test

binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
    ]

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
    liftError $ withModuleFromAST context newast $ \m -> do
        llstr <- moduleLLVMAssembly m
        putStrLn llstr
        return newast
    where
        modn = mapM codegenTop fns
        newast = runLLVM mod modn
