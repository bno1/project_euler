module Common where

import Prelude hiding (gcd)

import Data.Foldable
import Data.List (unfoldr)
import Control.Monad
import Control.Arrow (first)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

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

combinations :: (Integral a) => a -> a -> a
combinations n k = foldl' (\r i -> (r * (n - i + 1)) `quot` i) 1 [1..p]
  where
    p = min k (n - k)

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
  | n <= 1 || r /= 0 = (n, c)
  | otherwise = countFactorHelper k d $ c + 1
  where
    (d, r) = n `quotRem` k

countFactor :: (Integral a) => a -> a -> (a, Word)
countFactor k n
  | k < 2 = error "Factor smaller than 1"
  | n < 0 = error "Negative number"
  | otherwise = countFactorHelper k n 0

reverseNum :: (Integral a) => a -> a
reverseNum n = let
    step 0 b = b
    step a b = let (d, r) = a `quotRem` 10 in step d (b * 10 + r)
  in
    step n 0

isPalindrome :: (Integral a) => a -> Bool
isPalindrome n = n == reverseNum n

sieve :: Int -> [Int]
sieve n = let
    ub = n - 3
    --(lb, ub) = (2, fromIntegral n) :: (Word, Word)
    arr = VU.create $ do
      m <- VUM.replicate (ub + 1) True
      forM_ [0..ub] $ \i -> do
        v <- VUM.read m i
        when v $ forM_ [(i+2)*2,(i+2)*3..ub+2] $ \j -> VUM.write m (j - 2) False

      return m
  in
    VU.ifoldr (\i e next -> if e then i+2:next else next) [] arr

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

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn c = unfoldr (\s -> if null s then Nothing else Just (go s))
  where
    go [] = ([], [])
    go (x:xs) = if c == x then ([], xs) else (x:) `first` go xs
