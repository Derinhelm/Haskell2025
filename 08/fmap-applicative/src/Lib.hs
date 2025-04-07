module Lib
    ( someFunc
    ) where
    
funInContext1 :: Either String (Bool -> Bool)
funInContext1 = Right not

funInContext2 :: Either String (Bool -> Bool)
funInContext2 = Left "Error function"

valueInContext1 :: Either String Bool
valueInContext1 = Right True

valueInContext2 :: Either String Bool
valueInContext2 = Left "Error value"


someFunc :: IO ()
someFunc = do
    putStrLn $ "Функция без контекста, значение в контексте:"
    putStrLn $ show $ fmap not valueInContext1
    putStrLn $ show $ not <$> valueInContext1 -- <$> - инфиксный синоним fmap
    putStrLn $ show $ not <$> valueInContext2
    
    putStrLn $ "Функция в контексте, значение в контексте:"
    putStrLn $ show $ funInContext1 <*> valueInContext1
    putStrLn $ show $ funInContext1 <*> valueInContext2
    putStrLn $ show $ funInContext2 <*> valueInContext1
    putStrLn $ show $ funInContext2 <*> valueInContext2
    
