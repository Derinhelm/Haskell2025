module Lib
    ( someFunc
    ) where

createFromFunction :: (Int -> Int -> Int) -> Either String Int -> Either String (Int -> Int)
createFromFunction fun valueInContext = fun <$> valueInContext

createFromFunctionInContext :: Either String (Int -> Int -> Int) -> Either String Int -> Either String (Int -> Int)
createFromFunctionInContext fun valueInContext = fun <*> valueInContext

someFunc :: IO ()
someFunc = do
    let 
        f1 = createFromFunction (+) (Right 2) -- Изначальная функция без контекста
        f2 = createFromFunctionInContext (Right (+)) (Right 2) -- Изначальная функция с контекстом
    print $ f1 <*> (Right 3) -- f1 и f2 - в контексте, поэтому <*>
    print $ f2 <*> (Right 3)
    
    let
        r1 = ((+) <$> (Right 2)) <*> (Right 3)
        -- (+) без контекста, аналогично примеру выше
        r2 = (+) <$> (Right 2) <*> (Right 3) -- можно без скобок
    
        r3 = ((Right (+)) <*> (Right 2)) <*> (Right 3)
        -- (+) с контекстом, аналогично примеру выше
        r4 = (Right (+)) <*> (Right 2) <*> (Right 3) -- можно без скобок
        
    print ([r1, r2, r3, r4] :: ([Either String Int]))
