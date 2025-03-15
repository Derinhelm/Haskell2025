module Main (main) where

import MyData
import Lib

main :: IO ()
main = do
    putStrLn $ show $ eval (Not (And (Var "x") (Value True))) [("x", True)]
    putStrLn $ show $ eval (Not (And (Var "x") (Value True))) [("y", True)]
