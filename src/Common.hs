module Common where

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
