module Syntax where

type Name = String

data Expr
    = Float Double
    | BinaryOp Name Expr Expr
    | UnaryOp Name Expr
    | Var String
    | Call Name [Expr]
    | Function Name [Name] Expr
    | Extern Name [Name]
    deriving (Eq, Ord, Show)
