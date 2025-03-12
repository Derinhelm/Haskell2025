module MyData where

data Expr = Value Bool | And Expr Expr | Or Expr Expr | Not Expr | Var String
    deriving (Show)