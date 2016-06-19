module Syntax where

data Term
    = TmVar Info String
    | TmAbs Info String Type Term
    | TmApp Info Term Term
    | TmTrue Info
    | TmFalse Info
    | TmIf Info Term Term Term
    | TmBinOp Info Op Term Term 
    | TmInt Info Int
    | TmUnit Info
    deriving (Show, Eq)
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
    deriving (Eq)

instance Show Op where
    show Times = " * "
    show Plus = " + "

data Type
    = TyBool
    | TyInt
    | TyUnit
    | TyArrow Type Type
    deriving (Show, Eq)

data Info 
    = Info Int Int
    | DummyInfo
    deriving (Show, Eq)
