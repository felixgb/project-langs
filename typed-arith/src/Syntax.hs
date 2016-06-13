module Syntax where

data Exp = EIf Exp Exp Exp
    | ETrue
    | EFalse
    | EZero
    | ESucc Exp
    | EPred Exp
    | EIsZero Exp
    deriving (Show)

data Type = TyBool
    | TyInt
    deriving (Show, Eq)
