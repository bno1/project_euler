module P22
  ( solveP22
  ) where

import Common
import Data.List
import Data.Char (ord, toUpper)

rank :: Int -> String -> Int
rank r name = r * value
  where
    subA = subtract (ord 'A' - 1)
    value = sum $ map (subA . ord . toUpper) name

solveP22 :: IO Int
solveP22 = do
  rawNames <- readFile "files/p022_names.txt"
  let names = map read $ splitOn ',' rawNames

  return $ sum $ zipWith rank [1..] $ sort names
