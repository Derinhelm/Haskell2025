module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
    let 
        r1 = sequenceA [Right 3, Left "error1", Left "error2"]
        r2 = sequenceA [Right 3, Right 4]
        r3 = sequenceA [Just 2, Nothing, Just 3]
        r4 = sequenceA [Just 2, Just 3]
    print [r1, r2]
    print [r3, r4]
