module P24
  ( solveP24
  ) where

import Data.Char (intToDigit)

enum :: [a] -> [(a, [a])]
enum = go []
  where
    go _ [] = []
    go pref (x:xs) = (x, pref ++ xs) : go (pref ++ [x]) xs

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations v = enum v >>= (\(x, xs) -> map (x:) $ permutations xs)

solveP24 :: String
solveP24 = map intToDigit $ permutations [0..9] !! (1000000 - 1)
