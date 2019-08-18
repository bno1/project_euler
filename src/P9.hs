module P9
  ( solveP9
  ) where

import Common

selector :: Int
selector = 1000

solveP9 :: Int
solveP9 = a * b * c
  where
    discard (x, y, z) = selector `rem` (x + y + z) /= 0
    (a', b', c') = head $ dropWhile discard primPythTriplets
    k = selector `quot` (a' + b' + c')
    (a, b, c) = (k * a', k * b', k * c')
