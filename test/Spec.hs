import Test.Hspec
import Test.QuickCheck

import Data.Word
import Data.Foldable

import Lib
import Common
import Sieve

allPrimes :: [Word64]
allPrimes = 2:[p | p <- [3,5..], all (\i -> p `rem` i /= 0) $
               takeWhile (\i -> 2 * i < p) allPrimes]

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

    it "eliminateFactor" $ property $
      \n k -> (k /= 0 && n /= 0) ==> let
          m = eliminateFactor k (n :: Int)
          (d, r) = n `quotRem` m
        in
          r == 0 && eliminateFactor k d == 1

    it "reverseNum" $ property $
      \n -> let
          m = eliminateFactor 10 n
        in m == reverseNum (reverseNum (m :: Int))

  describe "Sieve" $ do
    it "stepSieve" $ forAll (choose (0, 10000)) $
      \n ->let
          sPrimes = toList $ sivPrimes $ runSieve (newSieve n)
        in all (uncurry (==)) $ zip allPrimes sPrimes

    it "runSieve" $ forAll (choose (0, 2000)) $ \n ->
      forAll (choose (1, 10)) $ \s -> let
          sPrimes = toList $ sivPrimes $ iterate runSieve (newSieve n) !! s
        in sPrimes == takeWhile (\p -> p < 3 + fromIntegral s * 2 * n) allPrimes

    it "nThPrime" $ forAll (choose (0, 1000)) $ \n ->
      allPrimes !! n == fst (nThPrime n (newSieve 100))

  describe "runProblem" $ do
    it "Fails gracefully" $ runProblem 999999 `shouldBe` "Unknown problem"
    it "Solves #1" $ runProblem 1 `shouldBe` "233168"
    it "Solves #2" $ runProblem 2 `shouldBe` "4613732"
    it "Solves #3" $ runProblem 3 `shouldBe` "6857"
    it "Solves #4" $ runProblem 4 `shouldBe` "906609"
    it "Solves #5" $ runProblem 5 `shouldBe` "232792560"
    it "Solves #6" $ runProblem 6 `shouldBe` "25164150"
