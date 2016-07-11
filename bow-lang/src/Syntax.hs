module Syntax where

import Control.Monad.Except
import qualified Data.Map as Map

type VarEnv = Map.Map String Expr

data Expr
    = EVar Info String
    | ELit Info Lit
    | ESeq Expr Expr
    | EUnit Info
    | EAssign Info String Expr
    | EFunction String [String] Expr VarEnv
    | EDef Info String [String] Expr
    | EInvoke Info String [Expr]
    | EBinexp Info Op Expr Expr
    | EIf Info Expr Expr Expr
    deriving (Show, Eq)

data Type
    = TyVar String
    | TyInt
    | TyBool
    | TyFunc [Type] Type
    | TyUnit
    deriving (Show, Eq)

data Op
    = Plus
    | Times
    | Equal
    deriving (Show, Eq)

data Lit
    = LInt Int
    | LBool Bool
    deriving (Show, Eq)

data Info 
    = Info Int Int
    | DummyInfo
    deriving (Show, Eq)

data LangErr
    = ErrParse String
    | ErrVarNotFound String
    | ErrCircularUnify String Type
    | ErrUnifyUnsolvable [(Type, Type)]
    deriving (Show)

type ThrowsError = Except LangErr

