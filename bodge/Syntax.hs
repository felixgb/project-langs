module Syntax where

import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Vector as V

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
    | ETaggedUnion Info String Type
    | ECase Info Expr [(Expr, Expr)]
    | ETag Info String [Expr]
    | EFold Info Type Expr
    | EUnfold Info Type Expr
    | EVector (V.Vector Expr)
    deriving (Show, Eq)

data Type
    = TyVar String
    | TyFunc [Type] Type
    | TyVector Type
    | TyInt
    | TyBool
    | TyUnit
    | TyTaggedUnion [(String, [Type])]
    | TyRec String Type
    deriving (Show, Eq, Ord)

data Scheme = Forall [Type] Type
    deriving (Show)

data Op
    = Plus
    | Times
    | Minus
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
    | ErrFieldMismatch
    | ErrNotInVariantFields String
    | ErrVarNotFound String
    | ErrTyVarNotFound String (Map.Map String Scheme)
    | ErrCircularUnify String Type
    | ErrUnifyUnsolvable [(Type, Type)]
    | ErrUnify [Type] [Type]

instance Show LangErr where
    show (ErrParse msg) = show msg
    show (ErrFieldMismatch) = "field mismatch"
    show (ErrNotInVariantFields msg) = "Not in union fields: " ++ msg
    show (ErrVarNotFound var ) = "Can't find variable: " ++ var
    show (ErrTyVarNotFound var env) = "Can't find type variable: " ++ var ++ ", env: " ++ (show env)
    show (ErrCircularUnify name ty) = "Circular constraints, var: " ++ name ++ " found in " ++ (show ty)
    show (ErrUnifyUnsolvable tys) = "Unable to unify types: " ++ (show tys)
    show (ErrUnify t1s t2s) = "Unable to unify types: " ++ (show t1s) ++ (show t2s)

type ThrowsError = Except LangErr

