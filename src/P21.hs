module P21
  ( solveP21
  ) where

import Data.Word
import Sieve
import qualified Control.Monad.State as St
import qualified Data.Vector.Unboxed as VU

computeSums :: Word64 -> VU.Vector Word64
computeSums bound = flip St.evalState (newSieve 1000) $
  VU.generateM (fromIntegral $ bound - 2)
    (fmap (sum . explodeFactors) . sivFactorize . fromIntegral . (+2))

findAmicables :: VU.Vector Word64 -> Word64 -> [Word64]
findAmicables v x
  | i >= VU.length v = []
  | j > i && j < VU.length v && amicable = x : y : rest
  | otherwise = rest
  where
    i = fromIntegral $ x - 2
    y = (v VU.! i) - x
    j = fromIntegral $ y - 2
    amicable = (v VU.! j) - y == x
    rest = findAmicables v (x + 1)

solveP21 :: Word64
solveP21 = sum $ findAmicables (computeSums 10000) 2
