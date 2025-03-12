module Main (main) where

import Lib
import System.IO

modify :: String -> String
modify s = s ++ "!"

printer :: String -> IO String
printer s = do
    putStrLn $ "Напечатали:" ++ s
    return $ modify "Успешно"

main :: IO ()
main = do
    content <- readFile "input.txt"
    result <- printer content
    putStrLn result
    -- putStrLn $ "В файле: " ++ content
    -- writeFile "output.txt" $ "Модифицировали:"++ content
