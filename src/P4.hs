module P4
  ( solveP4
  ) where

import Common

upperLimit :: (Integral a) => a
upperLimit = 999

lowerLimit :: (Integral a) => a
lowerLimit = 100

guess :: (Integral a) => a
guess = 111 * 111

findPal :: (Integral a) => a -> a -> a -> a
findPal a b p
  | a < lowerLimit = p
  | b < lowerLimit = findPal (a - 1) upperLimit p
  | q <= p = if b == upperLimit then p else findPal (a - 1) upperLimit p
  | isPalindrome q = findPal (a - 1) upperLimit q
  | otherwise = findPal a (b - 1) p
  where
    q = a * b

solveP4 :: Int
solveP4 = findPal upperLimit upperLimit guess
