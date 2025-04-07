module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
    let r1 = (pure (\z -> "+" ++ z)) <*> (Right "ere")
    print $ (r1 :: Either String String) -- Необходимо указать тип из-за неоднозначности выбора show
    print $ (pure (\z -> "+" ++ z)) <*> ["ere"]
    print $ (pure (\z -> "+" ++ z)) <*> ["ere", "rtrt"]
    print $ (pure (\z -> "+" ++ z)) <*> (Just "ere")
    
    let r2 = pure (not) <*> (Right False) -- Необходимо указать тип из-за неоднозначности выбора show
    print $ (r2 :: Either String Bool)
    print $ pure (not) <*> (Left "error")
