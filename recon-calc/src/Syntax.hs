module Syntax where

import Control.Monad.Except

data Term
    = TmVar Info String
    | TmAbs Info String (Maybe Type) Term
    | TmApp Info Term Term
    | TmTrue Info
    | TmFalse Info
    | TmIf Info Term Term Term
    | TmBinOp Info Op Term Term 
    | TmInt Info Int
    | TmIsZero Info Term
    | TmUnit Info
    -- Pair for now n-tuple later
    | TmPair Info [Term]
    | TmProj Info Term Int
    | TmCase Info Term [(String, (String, Term))]
    | TmTag Info String Term Type
    | TmDataDec Info String Type
    | TmFold Info Type Term
    | TmUnfold Info Type Term
    deriving (Show, Eq)

data Op
    = Times
    | Plus
    deriving (Eq)

instance Show Op where
    show Times = " * "
    show Plus = " + "

data Type
    = TyBool
    | TyInt
    | TyUnit
    | TyVariant [(String, Type)]
    | TyVar String
    | TyRecTy String Type
    | TyArrow Type Type
    | TyProd [Type]
    deriving (Show, Eq)

data Info 
    = Info Int Int
    | DummyInfo
    deriving (Show, Eq)

-- Error Handling 

data CalcError
    = ErrDefault String
    | ErrUnifyCircular String Type
    | ErrUnifyUnsolvable [(Type, Type)]
    | ErrNotProduct Type
    | ErrFieldMismatch String
    | ErrMissingLabel String
    | ErrNotVariant 
    | ErrNotRecTy
    | ErrTyVar String [(String, Type)]
    | ErrParse String

instance Show CalcError where
    show (ErrDefault msg) = "Default error ):" ++ msg
    show (ErrUnifyCircular name ty) = "circular constraints"
    show (ErrUnifyUnsolvable constrs) = "unsolvable constraints: " ++ (show constrs)
    show (ErrNotProduct ty) = "Not a product type: " ++ (show ty)
    show (ErrFieldMismatch str) = "Field Mismatch: " ++ str
    show (ErrMissingLabel str) = "Missing Label: " ++ str
    show (ErrNotVariant) = "Expected a variant type"
    show (ErrTyVar msg env) = "Missing type variable: " ++ msg ++ ", env: " ++ (show env)
    show (ErrParse msg) = msg

type ThrowsError = Except CalcError
