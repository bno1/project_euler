module P6
  ( solveP6
  ) where

import Common

limit :: (Integral a) => a
limit = 100

solveP6 :: Int
solveP6 = (ns * ns) - ns2
  where
    ns = nSum limit
    ns2 = nSum2 limit
