module Lib
    ( eval
    ) where

import MyData

eval :: Expr -> [(String, Bool)] -> Bool
eval (Value a) _ = a
eval (Not a) vars = not $ eval a vars
eval (Or a b) vars = (eval a vars) || (eval b vars)
eval (And a b) vars = (eval a vars) && (eval b vars)
eval (Var s) vars = case lookup s vars of 
                            Just w -> w
                            Nothing -> True