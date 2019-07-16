module P2
  ( solveP2
  ) where

import Common
import Data.Int

limit :: Int64
limit = 4000000

solveP2 :: Int64
solveP2 = fastFibSum k_3 `quot` 2
  where
    k = fastFibLt limit
    k_3 = k - (k `rem` 3)
