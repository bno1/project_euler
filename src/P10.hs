module P10
  ( solveP10
  ) where

import Sieve
import Data.Word (Word64)

bound :: Word64
bound = 2000000

primes :: [Word64]
primes = takeWhile (<bound) $ sivPrimesList $ runSieveUntil (newSieve 100000) bound

solveP10 :: Word64
solveP10 = sum primes
