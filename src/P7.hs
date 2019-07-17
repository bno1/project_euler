module P7
  ( solveP7
  ) where

import Data.Word

import Sieve

needle :: (Integral a) => a
needle = 10001

solveP7 :: Word64
solveP7 = fst $ nThPrime (needle - 1) $ newSieve 512
