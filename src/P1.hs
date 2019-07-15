module P1
  ( solveP1
  ) where

import Common

limit :: Int
limit = 1000

solveP1 :: String
solveP1 = let
    [sums3, sums5, sums15] = map (sumOfMults limit) [3, 5, 15]
  in
    show $ sums3 + sums5 - sums15
