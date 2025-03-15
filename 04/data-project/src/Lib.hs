module Lib
    ( eval
    ) where

import MyData

eval :: Expr -> [(String, Bool)] -> Either String Bool
eval (Value a) _ = Right a
eval (Not a) vars = case eval a vars of
                        Left e -> Left e
                        Right x -> Right $ not x
eval (Or a b) vars = binFun (||) (eval a vars) (eval b vars)
eval (And a b) vars = binFun (&&) (eval a vars) (eval b vars)
eval (Var s) vars = case lookup s vars of 
                        Just w -> Right w
                        Nothing -> Left $ "No var " ++ s

binFun :: (Bool -> Bool -> Bool) -> Either String Bool -> Either String Bool -> Either String Bool
binFun _ (Left e1) (Left e2) = Left $ e1 ++ ". " ++ e2
binFun _ (Left e1) _ = Left $ e1
binFun _ _ (Left e2) = Left $ e2
binFun f (Right x1) (Right x2) = Right $ f x1 x2
