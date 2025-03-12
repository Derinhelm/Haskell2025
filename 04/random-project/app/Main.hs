module Main (main) where

import MyData
import System.Random

main :: IO ()
main = do
   gen <- getStdGen
   putStrLn $ generateList gen 30
