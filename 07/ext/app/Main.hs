{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

data MyRecord = MyRecord { field1 :: Int
                         , field2 :: String
                         } deriving Show

-- ScopedTypeVariables
f :: MyRecord -> String
f (MyRecord (f1 :: Int) (f2 :: String)) = show f1 ++ ", " ++ f2

-- RecordWildCards
g :: MyRecord -> String
g rec@MyRecord{..} = show (field1 :: Int) ++ ", " ++ (field2 :: String) ++ ", " ++ 
                     show (rec :: MyRecord)

-- GeneralizedNewtypeDeriving
newtype RUB = RUB Double deriving (Show, Num)

-- TupleSections
tuples :: a -> b -> [c] -> [(a, c, b)]
tuples x y zs = map (x ,, y) zs

h :: (String, Int, String)
h = (,2,) "str1" "str2"
--  (\x y -> (x, 2, y))

-- LambdaCase
fromMaybeList :: a -> [Maybe a] -> [a]
fromMaybeList def = map
  (\case
    Just x -> x
    _      -> def
  )
  
main :: IO ()
main = do
    putStrLn $ show $ f $ MyRecord 10 "My text"
    putStrLn $ show $ g $ MyRecord 10 "My text"
    putStrLn $ show $ tuples "First" "Second" ["Text1", "Text2", "Text3"]
    putStrLn $ show h
    putStrLn $ show $ fromMaybeList "Default Text" [Just "Text 1", Nothing]
