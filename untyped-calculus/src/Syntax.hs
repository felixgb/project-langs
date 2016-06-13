module Syntax where

data Exp
    = EIf Exp Exp Exp
    | ETrue
    | EFalse
    | EZero
    | ESucc Exp
    | EPred Exp
    | EIsZero Exp
    deriving (Show)

data Binding = NameBind deriving (Show, Eq)


data Term
    -- First int is the de bruijn index of the variable, second is how many
    -- abstractions are stored in the scope of the variable
    = TmVar Info Int Int
    | TmAbs Info String Term
    | TmApp Info Term Term
    deriving (Show)

data Info = Info Int Int deriving (Show)

type Context = [(String, Binding)]
