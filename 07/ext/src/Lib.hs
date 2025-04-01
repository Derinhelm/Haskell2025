{-# LANGUAGE RecordWildCards #-}

module Lib
    ( someFunc
    ) where

data MyRecord = MyRecord
              { field1 :: Int
              , field2 :: String
              } deriving Show

g :: MyRecord -> String
g x@MyRecord{..} = show (field1) ++ "\n" ++ (field2) ++ "\n" ++ show (x)

someFunc :: IO ()
someFunc = putStrLn $ g $ MyRecord 5 "Hello"
