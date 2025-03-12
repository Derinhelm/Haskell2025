module Lib
    ( generateList
    ) where

import System.Random


generateList :: StdGen -> Int -> [Char]
generateList gen len = take len $ (randomRs ('a', 'z') gen)