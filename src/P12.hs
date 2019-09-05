module P12
  ( solveP12
  ) where

import Sieve
import Data.Word
import qualified Control.Monad.State as St


triangleNumbers :: [Word64]
triangleNumbers = go 1
  where
    go n = ((n * 2 - 1) * n) : ((n * 2 + 1) * n) : go (n + 1)

findNumber :: Word -> [Word64] -> Word64
findNumber cnt nums =
  St.evalState (foldr f (error "impossible") nums) (newSieve 1000)
  where
    divCount = product . fmap ((+1) . snd)
    f n next = do
      fs <- sivFactorize n
      if divCount fs > cnt then return n else next

solveP12 :: Word64
solveP12 = findNumber 500 triangleNumbers
