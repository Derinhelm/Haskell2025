module Lib
    ( someFunc
    ) where

newtype MyAssocListK k v = MyAssocListK [(k, v)] deriving Show -- Сначала k, потом v
instance Functor (MyAssocListK v) where
    fmap f (MyAssocListK a) = MyAssocListK (map (\x -> (fst x, f $ snd x)) a)
    
newtype MyAssocListV v k = MyAssocListV [(k, v)] deriving Show -- Сначала v, потом k
instance Functor (MyAssocListV v) where
    fmap f (MyAssocListV a) = MyAssocListV (map (\x -> (f $ fst x, snd x)) a) 

someFunc :: IO ()
someFunc = do
    let a = MyAssocListK [(1, "UwU"), (2, "0w0"), (3, "=w=")]
        b = MyAssocListV [(1, "UwU"), (2, "0w0"), (3, "=w=")]
    putStrLn $ show a
    putStrLn $ show $ fmap (\x -> "^" ++ x ++ "^") a

    putStrLn $ show b
    putStrLn $ show $ fmap (+ 1) b
