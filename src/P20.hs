module P20
  ( solveP20
  ) where

digitSum :: (Integral a) => a -> Word
digitSum 0 = 0
digitSum n = fromIntegral r + digitSum q
  where
    (q, r) = n `quotRem` 10

solveP20 :: Word
solveP20 = digitSum $ foldl (*) (1 :: Integer) [2..100]
