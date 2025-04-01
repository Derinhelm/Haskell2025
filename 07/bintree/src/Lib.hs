module Lib
    ( someFunc
    ) where

import Data.Monoid

data BinTree a = Node a (BinTree a) (BinTree a) | Leaf
    deriving Show

instance Foldable BinTree where
    foldMap _ Leaf = mempty
    foldMap f (Node v l r) = foldMap f l <> f v <> foldMap f r 

someFunc :: IO ()
--someFunc = putStrLn $ show $ foldMap (\x -> " ^" ++ x ++ "^ ") (Node "0w0" (Node ">w<" Leaf Leaf) Leaf) 
someFunc = do
    v <- getLine
    let
        tree = (Node 4 (Node 2 Leaf Leaf) Leaf)
        res =  case v of
                 "*" -> show $ foldMap Product tree
                 "+" -> show $ foldMap Sum tree
                 _ -> "Error"
    putStrLn res
    
  