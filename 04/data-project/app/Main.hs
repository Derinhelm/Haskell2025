module Main (main) where

import MyData
import Lib

main :: IO ()
main = putStrLn $ show $ eval (Not (And (Var "x") (Value True))) [("y", True)]
