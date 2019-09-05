module P3
  ( solveP3
  ) where

import Common
import Data.Int

primeFactors :: (Integral a) => a -> [a]
primeFactors n = let
    limit = floor $ sqrt (fromIntegral n :: Double)
    step 1 [] = []
    step i [] = [i]
    step i (k:ks)
      | k > i = []
      | i `rem` k == 0 = k:step (fst $ countFactor k i) ks
      | otherwise = step i ks
  in
    step n $ 2:[3,5..limit]

solveP3 :: Int64
solveP3 = last $ primeFactors 600851475143
