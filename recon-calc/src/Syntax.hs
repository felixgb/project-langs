module Syntax where

import Control.Monad.Except

data Term
    = TmVar Info String
    | TmAbs Info String Type Term
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
    | ErrUnify String
    | ErrTyVar String
    | ErrParse

instance Show CalcError where
    show (ErrDefault msg) = msg
    show (ErrUnify msg) = msg
    show (ErrTyVar msg) = msg
    show (ErrParse) = "parse error"

type ThrowsError a = Either CalcError a

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
