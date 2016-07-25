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
    | ECallShell Info Expr Expr
    | EPrint Info Expr
    | EInvoke Info String [Expr]
    | EBinexp Info Op Expr Expr
    | EIf Info Expr Expr Expr
    | EFor Info Expr Expr Expr
    | ETaggedUnion Info [Type] String Type
    | ECase Info Expr [(Expr, Expr)]
    | ETag Info String [Expr]
    | EVector Info (V.Vector Expr)
    | EIndex Info String Expr
    deriving (Show, Eq)

-- builtins = Map.fromList [(print

data Type
    = TyVar String
    | TyFunc [Type] Type
    | TyVector Type
    | TyInt
    | TyBool
    | TyUnit
    | TyString
    | TyTaggedUnion [(String, [Type])]
    | TyRec String Type
    | TyVec Type
    deriving (Show, Eq)

data Kind
    = KStar
    | KArr Kind Kind

data Op
    = Plus
    | Times
    | Minus
    | Equal
    deriving (Show, Eq)

data Lit
    = LInt Int
    | LString String
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
    | ErrTyVarNotFound String (Map.Map String Type)
    | ErrCircularUnify String Type
    | ErrUnifyUnsolvable [(Type, Type)]
    | ErrIndexOutOfBounds Int String

instance Show LangErr where
    show (ErrParse msg) = show msg
    show (ErrFieldMismatch) = "field mismatch"
    show (ErrNotInVariantFields msg) = "Not in union fields: " ++ msg
    show (ErrVarNotFound var ) = "Can't find variable: " ++ var
    show (ErrTyVarNotFound var env) = "Can't find type variable: " ++ var ++ ", env: " ++ (show env)
    show (ErrCircularUnify name ty) = "Circular constraints, var: " ++ name ++ " found in " ++ (show ty)
    show (ErrUnifyUnsolvable tys) = "Unable to unify types: " ++ (show tys)
    show (ErrIndexOutOfBounds idx name) = "Index out of bounds: " ++ (show idx) ++ ", " ++ name
type ThrowsError = Except LangErr 

