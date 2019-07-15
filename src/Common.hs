module Common where

nSum :: (Integral a) => a -> a
nSum n = n * (n + 1) `quot` 2

sumOfMults :: (Integral a) => a -> a -> a
sumOfMults limit n = n * nSum ((limit - 1) `quot` n)
