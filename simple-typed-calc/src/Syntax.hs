module Syntax where

data Binding = NameBind deriving (Show, Eq)

data Term
    -- First int is the de bruijn index of the variable, second is how many
    -- abstractions are stored in the scope of the variable
    = TmVar Info Int Int
    | TmAbs Info String Term
    | TmApp Info Term Term
    | TmTrue Info
    | TmFalse Info
    | TmIf Info Term Term Term
    | TmBinOp Info Op Term Term 
    | TmInt Info Int
    deriving (Show)
--
--instance Show Term where
--    show (TmInt info n) = show n
--    show (TmTrue info) = "true"
--    show (TmFalse info) = "false"
--    show (TmIf info cond t1 t2) = "if..."
--    show (TmBinOp info op t1 t2) = (show t1) ++ (show op) ++ (show t2)

data Op
    = Times
    | Plus

instance Show Op where
    show Times = " * "
    show Plus = " + "

data Type
    = TyBool
    | TyArrow Type Type

data Info = Info Int Int deriving (Show)

type Context = [(String, Binding)]
