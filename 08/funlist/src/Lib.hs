module Lib
    ( someFunc
    ) where

import Data.Char

addName :: String -> Either String (String -> String)
addName s | (isUpper $ head s) = Right $ \x -> s ++ ":" ++ x
          | otherwise = Left $ "Error name: " ++ s

someFunc :: IO ()
someFunc = do
    {-let funElena = addName "Elena"
        funList = [ addName "elena", addName "Ivan", addName "Sergey"]
        texts = ["Text1", "Text2"]
    putStrLn $ show $ sequenceA $ foldr1 (++) $ map (\f -> map (\v -> f <*> (pure v)) texts) funList
    -}--putStrLn $ show $ funElena <*> (pure "Text2")

    let funElena = addName "elena"
        funIvan = addName "Ivan"
    putStrLn $ show $ sequenceA $ [funElena <*> (pure "Text1"),
                                   funIvan <*> (pure "Text1")]
    --putStrLn $ show $ funElena <*> (pure "Text2")
