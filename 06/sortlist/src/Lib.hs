module Lib
    ( someFunc
    ) where

import Data.Monoid

newtype SortedList a = SortedList [a] deriving Show
instance (Ord a) => Semigroup (SortedList a) where
    (<>) (SortedList []) y = y
    (<>) x (SortedList []) = x
    (<>) (SortedList (x1:x)) (SortedList (y1:y))
        | (x1<y1) = myCons x1 ((<>) (SortedList x) (SortedList (y1:y)))
        | otherwise = myCons y1 ((<>) (SortedList (x1:x)) (SortedList y))

instance (Ord a) => Monoid (SortedList a) where
    mempty = (SortedList [])


myCons:: a -> (SortedList a) -> (SortedList a)
myCons t1 (SortedList t)= SortedList (t1:t)

someFunc :: IO ()
someFunc = do
    putStrLn $ show $ (<>) (SortedList [1,3,5]) (SortedList [2, 4, 6])
