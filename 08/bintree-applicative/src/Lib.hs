module Lib
    ( someFunc
    ) where

data BinTree a = Node a (BinTree a) (BinTree a) | Leaf
    deriving Show

instance Foldable BinTree where
    foldMap _ Leaf = mempty
    foldMap f (Node v l r) = foldMap f l <> f v <> foldMap f r 

parseFun :: String -> Either String (Int -> Int -> Int)
parseFun "*" = Right (*)
parseFun "+" = Right (+)
parseFun f = Left $ "Wrong function: " ++ f

someFunc :: IO ()
someFunc = do
    putStrLn "Choose function: * or +"
    fStr <- getLine
    --putStrLn "Type first value:"
    --s_val1 <- getLine
    --putStrLn "Type second value"
    --s_val2 <- getLine
    let pf = parseFun fStr -- :: Either String (a->a->a)
    putStrLn $ show $ pf <*> (Right 4) <*> (Right 2)
