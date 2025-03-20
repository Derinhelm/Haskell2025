module Lib
    ( someFunc
    ) where

newtype MyLast a = MyLast (Maybe a) deriving Show
instance Semigroup (MyLast a) where
    (<>) x (MyLast Nothing) = x
    (<>) _ y = y

instance Monoid (MyLast a) where
    mempty = MyLast Nothing


someFunc :: IO ()
someFunc = do
    
    putStrLn $ show $ foldr1 (<>) [(MyLast (Just 3)), (MyLast Nothing), (MyLast (Just 5))]
