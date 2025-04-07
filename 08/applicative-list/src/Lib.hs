module Lib
    ( someFunc
    ) where

funList1 :: [String -> String] -- Контекст - список
funList1 = [\x -> x ++ "!", \y -> y ++ "?"]

valueList1 :: [String] -- Контекст - список
valueList1 = ["Text1", "Text2"]

funList2 :: [Int -> [Int]]
funList2 = [(replicate 1), (replicate 3), \x -> [x, 2 * x, 3 * x]]

valueList2 :: [Int]
valueList2 = [2, 3]

someFunc :: IO ()
someFunc = do
    putStrLn $ show $ funList1 <*> valueList1 -- И функция, и значение - контекстные, поэтому <*>
    putStrLn $ show $ funList2 <*> valueList2
