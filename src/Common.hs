module Common where

import Prelude hiding (gcd)
import Control.Monad
import Data.Array.ST (runSTUArray)
import qualified Data.Array.MArray as MA
import qualified Data.Array.IArray as IA

goldenRatio :: Double
goldenRatio = 1.6180339887498948482045868343656381177203091798057628621354486

sqrt5 :: Double
sqrt5 = sqrt 5

fastFibLimit :: (Integral a) => a
fastFibLimit = 76

nSum :: (Integral a) => a -> a
nSum n
  | n > 0 = n * (n + 1) `quot` 2
  | otherwise = 0

nSum2 :: (Integral a) => a -> a
nSum2 n
  | n > 0 = n * (n + 1) * (2 * n + 1) `quot` 6
  | otherwise = 0

sumOfMults :: (Integral a) => a -> a -> a
sumOfMults limit n
  | limit <= 0 = 0
  | otherwise = n * nSum ((limit - 1) `quot` n)

fastFib :: (Integral a) => a -> a
fastFib n
  | n < 0 || n >= fastFibLimit = error "n too big"
  | otherwise = round ((goldenRatio ^ n) / sqrt5)

approxFibLt :: (Integral a) => a -> a
approxFibLt n = floor $ (lns5 + lnn) / lngr
  where
    lns5 = log sqrt5
    lngr = log goldenRatio
    lnn = log $ fromIntegral n

fastFibLt :: (Integral a) => a -> a
fastFibLt n = if fastFib k >= n then k - 1 else k
  where
    k = approxFibLt n

fastFibSum :: (Integral a) => a -> a
fastFibSum n = fastFib (n + 2) - 1

countFactorHelper :: (Integral a) => a -> a -> Word -> (a, Word)
countFactorHelper k n c
  | r == 0 = countFactorHelper k d $ c + 1
  | otherwise = (n, c)
  where
    (d, r) = n `quotRem` k

countFactor :: (Integral a) => a -> a -> (a, Word)
countFactor k n
  | k < 2 = error "Factor smaller than 1"
  | n < 2 = error "Negative or zero number"
  | otherwise = countFactorHelper k n 0

reverseNum :: (Integral a) => a -> a
reverseNum n = let
    step 0 b = b
    step a b = let (d, r) = a `quotRem` 10 in step d (b * 10 + r)
  in
    step n 0

isPalindrome :: (Integral a) => a -> Bool
isPalindrome n = n == reverseNum n

sieve :: (Integral a) => a -> [a]
sieve n = let
    (lb, ub) = (2, fromIntegral n) :: (Word, Word)
    arr = runSTUArray $ do
      m <- MA.newArray (lb, ub) True
      forM_ [lb..ub] $ \i -> do
        v <- MA.readArray m i
        when v $ forM_ [i*2,i*3..ub] $ \j -> MA.writeArray m j False

      return m
  in
    [ fromIntegral i | (i, e) <- IA.assocs arr, e]

gcd :: (Integral a) => a -> a -> a
gcd a b
  | b == 0 = a
  | otherwise = gcd b (a `rem` b)

primPythTripletsGen :: (Integral a) => a -> a -> [(a, a, a)]
primPythTripletsGen m n
  | n >= m = primPythTripletsGen (m + 1) (if even m then 2 else 1)
  | gcd m n == 1 = (m * m - n * n, 2 * m * n, m * m + n * n) :
                   primPythTripletsGen m (n + 2)
  | otherwise = primPythTripletsGen m (n + 2)

primPythTriplets :: (Integral a) => [(a, a, a)]
primPythTriplets = primPythTripletsGen 2 1

iSqrt :: (Integral a) => a -> a
iSqrt = ceiling . (sqrt :: Double -> Double) . fromIntegral
