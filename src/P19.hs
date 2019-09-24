module P19
  ( solveP19
  ) where

monthDays :: Int -> Int -> Int
monthDays m y
  | m `elem` [4, 6, 9, 11] = 30
  | m == 2 = if y `quot` 100 == 0
             then if y `quot` 400 == 0 then 29 else 28
             else if y `quot` 4 == 0 then 29 else 28
  | otherwise = 31

iter :: (Int, Int, Int) -> (Int, Int, Int)
iter (m, y, o)
  | m == 12 = (1, y + 1, o')
  | otherwise = (m + 1, y, o')
  where
    o' = (o + monthDays m y) `rem` 7

solveP19 :: Int
solveP19 = length $
           filter (\(_, _, o) -> o == 0) $
           takeWhile (\(_, y, _) -> y <= 2000) $
           dropWhile (\(_, y, _) -> y < 1901) $
           iterate iter (1, 1900, 1)
