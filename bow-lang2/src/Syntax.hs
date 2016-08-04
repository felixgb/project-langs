module Syntax where

import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Vector as V

data Expr 
    = EVar String
    | EInvoke Expr [Type] [Expr]
    | EInt Int
    | EBool Bool
    | ESeq Expr Expr
    | EAssign String Type Expr
    | EBinop Op Expr Expr
    | FunDecl String [String] [(String, Type)] Type Expr
    | TypeDecl String [String] Type
    deriving (Show, Eq)

-- builtins = Map.fromList [(print

data Type
    = TyVar String              -- Type Identifier
    | TyPoly [String] Type      -- Polymorphic type
    | TyApp Tycon [Type]
    deriving (Show, Eq, Ord)    

data Tycon
    = TyInt
    | TyBool
    | TyUnit
    | TyArrow
    deriving (Show, Eq, Ord)    

data Op = Plus
        | Times
        | Minus
        | Equal
        deriving (Show, Eq)

data Lit = LInt Int
         | LString String
         | LBool Bool
         deriving (Show, Eq)

data Info = Info Int Int
          | DummyInfo
          deriving (Show, Eq)

data LangErr = ErrParse String
             | ErrFieldMismatch
             | ErrNotInVariantFields String
             | ErrVarNotFound String
             | ErrCircularUnify String Type
             | ErrUnifyUnsolvable [(Type, Type)]
             | ErrIndexOutOfBounds Int String
             | ErrUnify [Type] [Type]
             | ErrExpectedKindArr
             | ErrDefault String

instance Show LangErr where
    show (ErrParse msg) = show msg
    show (ErrFieldMismatch) = "field mismatch"
    show (ErrNotInVariantFields msg) = "Not in union fields: " ++ msg
    show (ErrVarNotFound var ) = "Can't find variable: " ++ var
    show (ErrCircularUnify name ty) = "Circular constraints, var: " ++ name ++ " found in " ++ (show ty)
    show (ErrUnifyUnsolvable tys) = "Unable to unify types: " ++ (show tys)
    show (ErrIndexOutOfBounds idx name) = "Index out of bounds: " ++ (show idx) ++ ", " ++ name
    show (ErrUnify t1s t2s) = "Unable to unify types: " ++ (show t1s) ++ (show t2s)
    show (ErrDefault msg) = msg

type ThrowsError = Except LangErr 
