module Main (main) where

import Lib

main :: IO ()
main = do
    putStrLn "Как Вас зовут?"
    name <- getLine
    let newStr = "Привет, " ++ name ++ "!"
    putStrLn $ newStr