import Prelude hiding (gcd)

import Test.Hspec
import Test.QuickCheck

import Data.Word
import Data.Foldable

import Lib
import Common
import Sieve

import qualified Control.Monad.State as St

allPrimes :: [Word64]
allPrimes = 2:[p | p <- [3,5..], all (\i -> p `rem` i /= 0) $
               takeWhile (\i -> 2 * i < p) allPrimes]

first10PrimPythTrips :: [(Int, Int, Int)]
first10PrimPythTrips = [
  (3, 4, 5), (5, 12, 13), (15, 8, 17), (7, 24, 25), (21, 20, 29), (9, 40, 41),
  (35, 12, 37), (11, 60, 61), (45, 28, 53), (33, 56, 65), (13, 84, 85),
  (63, 16, 65), (55, 48, 73), (39, 80, 89), (15, 112, 113), (77, 36, 85)
  ]


iterateNM :: (Applicative f) => Int -> f a -> f a
iterateNM n f
  | n == 1 = f
  | n < 1 = error "impossible"
  | otherwise = f *> iterateNM (n - 1) f


main :: IO ()
main = hspec $ do
  describe "Common functions" $ do
    it "nSum" $ property $ \n -> nSum (n :: Int) == sum [1..n]
    it "nSum2" $ property $ \n ->
      nSum2 (n :: Int) == sum (map (\i -> i*i) [1..n])

    it "sumOfMults" $ property $
      \n lim -> (n > 0) ==>
        sumOfMults lim (n :: Word) == sum (takeWhile (<lim) [n,2*n..])

    it "fastFib Base Case" $ map fastFib [0, 1, 2] `shouldBe` [0, 1, 1 :: Int]

    it "fastFib Induction" $ forAll (choose (2, fastFibLimit - 1)) $
      \n -> fastFib (n :: Word) == fastFib (n - 1) + fastFib (n - 2)

    it "fastFibSum" $ forAll (choose (2, fastFibLimit - 3)) $
      \n ->fastFibSum (n :: Word) == sum (map fastFib [1..n])

    it "approxFibLt" $ forAll (choose (2, fastFib (fastFibLimit - 1) - 1)) $
      \n -> let
          k = approxFibLt (n :: Int)
        in fastFib k <= n && fastFib (k + 1) >= n

    it "fastFibLt" $ forAll (choose (2, fastFib (fastFibLimit - 1) - 1)) $
      \n -> let
          k = fastFibLt (n :: Int)
        in fastFib k < n && fastFib (k + 1) >= n

    it "countFactor" $ forAll (choose (0, 100000)) $ \n ->
      forAll (choose (2, 100000)) $ \k -> let
          (d, c) = countFactor k (n :: Int)
        in
          d * k ^ c == n && countFactor k d == (d, 0)

    it "reverseNum" $ property $
      \n -> let
          m = if n > 1 then fst (countFactor 10 n) else n
        in m == reverseNum (reverseNum (m :: Word))

    it "gcd" $ property $
      \a b -> let
          c = gcd a (b :: Int)
        in (a == 0 && b == 0 && c == 0) || (a `rem` c == 0 && b `rem` c == 0)

    it "primPythTriplets 16" $ first10PrimPythTrips == take 16 primPythTriplets

    it "primPythTriplets many" $ let
        trips = take 100000 primPythTriplets :: [(Int, Int, Int)]
        checkTrip (a, b, c) = a * a + b * b == c * c
        checkPrim (a, b, c) = gcd a (gcd b c) == 1
      in all (\trip -> checkTrip trip && checkPrim trip) trips

    it "splitOn empty" $ null $ splitOn ' ' []
    it "splitOn string" $ splitOn ' ' " a b c   d " == ["", "a", "b", "c", "", "", "d"]

  describe "Sieve" $ do
    it "stepSieve" $ forAll (choose (0, 10000)) $
      \n -> let
          sPrimes = toList $ sivPrimes $ St.execState runSieve (newSieve n)
        in all (uncurry (==)) $ zip allPrimes sPrimes

    it "runSieve" $ forAll (choose (0, 2000)) $ \n ->
      forAll (choose (1, 10)) $ \s -> let
          sPrimes = toList $ sivPrimes $ St.execState (iterateNM s runSieve) (newSieve n)
        in sPrimes == takeWhile (\p -> p < 3 + fromIntegral s * 2 * n) allPrimes

    it "nThPrime" $ forAll (choose (0, 1000)) $ \n ->
      allPrimes !! n == St.evalState (nThPrime n) (newSieve 100)

    it "runSieveUntil" $ forAll (choose (0, 2000)) $ \b ->
      forAll (choose (0, b * 100)) $ \n ->
        n <= sivPosition (St.execState (runSieveUntil n) (newSieve b))

  describe "runProblem" $ do
    it "Fails gracefully" $ runProblem 999999 `shouldReturn` "Unknown problem"
    it "Solves #1" $ runProblem 1 `shouldReturn` "233168"
    it "Solves #2" $ runProblem 2 `shouldReturn` "4613732"
    it "Solves #3" $ runProblem 3 `shouldReturn` "6857"
    it "Solves #4" $ runProblem 4 `shouldReturn` "906609"
    it "Solves #5" $ runProblem 5 `shouldReturn` "232792560"
    it "Solves #6" $ runProblem 6 `shouldReturn` "25164150"
    it "Solves #7" $ runProblem 7 `shouldReturn` "104743"
    it "Solves #8" $ runProblem 8 `shouldReturn` "23514624000"
    it "Solves #9" $ runProblem 9 `shouldReturn` "31875000"
    it "Solves #10" $ runProblem 10 `shouldReturn` "142913828922"
    it "Solves #11" $ runProblem 11 `shouldReturn` "70600674"
    it "Solves #12" $ runProblem 12 `shouldReturn` "76576500"
    it "Solves #13" $ runProblem 13 `shouldReturn` "5537376230"
    it "Solves #14" $ runProblem 14 `shouldReturn` "837799"
    it "Solves #15" $ runProblem 15 `shouldReturn` "137846528820"
    it "Solves #16" $ runProblem 16 `shouldReturn` "1366"
    it "Solves #17" $ runProblem 17 `shouldReturn` "21124"
    it "Solves #18" $ runProblem 18 `shouldReturn` "1074"
    it "Solves #19" $ runProblem 19 `shouldReturn` "171"
    it "Solves #20" $ runProblem 20 `shouldReturn` "648"
    it "Solves #21" $ runProblem 21 `shouldReturn` "31626"
    it "Solves #22" $ runProblem 22 `shouldReturn` "871198282"
    it "Solves #23" $ runProblem 23 `shouldReturn` "4179871"
    it "Solves #24" $ runProblem 24 `shouldReturn` "2783915460"
