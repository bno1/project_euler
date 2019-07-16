module P5
  ( solveP5
  ) where

import Common

limit :: (Integral a) => a
limit = 20

primeMaxFactor :: (Integral a) => a -> a -> a
primeMaxFactor p lim = floor $ llim / lp
  where
    lp = log (fromIntegral p :: Double)
    llim = log (fromIntegral lim :: Double)

solveP5 :: Int
solveP5 = product $ map (\p -> p ^ primeMaxFactor p limit) $ sieve limit
