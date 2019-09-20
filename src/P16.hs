module P16
  ( solveP16
  ) where

import Data.Bits
import Data.Word

-- Implement long division for bignums because it's fun and interestring

type Digit = Word32
type LDigit = Word64    -- used for operations
type Number = [Digit]

digitBits :: Int
digitBits = finiteBitSize (0 :: Digit)

doLongDiv :: Number -> Digit -> LDigit -> (Number, Digit)
doLongDiv [] d b = ([fromIntegral q], fromIntegral r)
  where
    (q, r) = b `quotRem` fromIntegral d

doLongDiv (x:xs) d b
  | b < dl = doLongDiv xs d (newB b)
  | otherwise = (fromIntegral qb:q, r)
  where
    dl = fromIntegral d

    -- newB a = (a << digitBits) | x
    newB = (fromIntegral x .|.) . (`shiftL` digitBits)

    (qb, rb) = b `quotRem` dl
    (q, r) = doLongDiv xs d (newB rb)

pow2 :: Int -> Number
pow2 n = (1 `shiftL` i) : replicate m 0
  where (m, i) = n `quotRem` digitBits

digitSum :: Number -> Word64 -> Word64
digitSum [0] s = s
digitSum n s = digitSum q (s + fromIntegral r)
  where (q, r) = doLongDiv n 10 0

solveP16 :: Word64
solveP16 = digitSum (pow2 1000) 0
