module P23
  ( solveP23
  ) where

import Sieve
import qualified Control.Monad.State as St
import qualified Data.Vector.Unboxed as VU

bound :: Int
bound = 28123

abundantsTable :: VU.Vector Bool
abundantsTable = flip St.evalState (newSieve 10000) $
  VU.generateM bound $ (\n -> (>2*n) . sum . explodeFactors <$> sivFactorize n) . fromIntegral

abundants :: [Int]
abundants = filter (abundantsTable VU.!) [1..bound]

checkNumber :: Int -> Bool
checkNumber n = any match $ takeWhile (<= n `quot` 2) abundants
  where
    match k = abundantsTable VU.! (n - k)

solveP23 :: Int
solveP23 = sum $ filter (not . checkNumber) [1..bound]
