module Lib
    ( someFunc
    ) where

data RoseTree a = RoseTree a [RoseTree a] deriving Show
instance Functor RoseTree where
    --fmap  :: (a -> b) -> RoseTree a -> RoseTree b
    fmap f (RoseTree v  children) = RoseTree (f v) (map (fmap f) children)

-- fmap f :: * -> *
-- fmap f :: RoseTree a -> RoseTree b

someFunc :: IO ()
someFunc = do
    let rtree = RoseTree 2 [RoseTree 4 [], RoseTree 6 [], RoseTree 0 []]
    putStrLn $ show rtree
    putStrLn $ show (fmap (> 2) rtree)

