module Lib
    ( someFunc
    ) where

data Grid a = Grid ((Int, Int) -> a)
instance Functor Grid where
    fmap f (Grid g) = Grid (\x -> f(g x))

gridCreator1 :: Grid String -- то есть Grid ((Int, Int) -> String)
gridCreator1 = Grid gridFun
    where
        gridFun :: (Int, Int) -> String
        gridFun (0, 0) = "The origin"
        gridFun (0, _) = "The y axis"
        gridFun (_, 0) = "The x axis"
        gridFun (_, _) = "Not the axis"

gridApply :: Grid a -> (Int, Int) -> a
gridApply (Grid f) coord = f coord

someFunc :: IO ()
someFunc = do  
    putStrLn $ show $ gridApply gridCreator1 (0, 0)
    putStrLn $ show $ gridApply gridCreator1 (1, 0)
    putStrLn $ show $ gridApply gridCreator1 (1, 2)
    putStrLn ""
    let newGrid1 = fmap (\t -> "Result: " ++ t) gridCreator1
        newGrid2 = fmap (\t -> "Our result: " ++ t ++ "!") gridCreator1
    putStrLn $ show $ gridApply newGrid1 (1, 2)
    putStrLn $ show $ gridApply newGrid2 (1, 2)
