module Lib
    ( someFunc
    ) where

import Data.Monoid

someFunc :: IO ()
someFunc = do
    putStrLn "First example:"
    putStrLn $ show $ getSum $ mconcat $ map Sum [1, 3, 5]
    putStrLn $ show $ getSum $ foldMap Sum [1, 3, 5]
    putStrLn "Second example:"
    putStrLn $ show $ replicate 3 2
    putStrLn $ show $ mconcat $ map (replicate 3) [1, 3, 5]
    putStrLn $ show $ foldMap (replicate 3) [1, 3, 5]
