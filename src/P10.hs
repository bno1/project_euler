module P10
  ( solveP10
  ) where

import Sieve
import Data.Word (Word64)
import qualified Control.Monad.State as St

bound :: Word64
bound = 2000000

sieve :: Sieve
sieve = St.execState (runSieveUntil bound) $ newSieve 100000

primes :: [Word64]
primes = takeWhile (<bound) $ sivPrimesList sieve

solveP10 :: Word64
solveP10 = sum primes
