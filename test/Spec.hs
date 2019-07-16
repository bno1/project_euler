import Test.Hspec
import Test.QuickCheck
import Lib
import Common

main :: IO ()
main = hspec $ do
  describe "Common functions" $ do
    it "nSum" $ property $ \n -> nSum (n :: Int) == sum [1..n]

    it "sumOfMults" $ property $
      \n lim -> (n > 0) ==>
        sumOfMults lim (n :: Word) == sum (takeWhile (<lim) [n,2*n..])

    it "fastFib Base Case" $ map fastFib [0, 1, 2] `shouldBe` [0, 1, 1 :: Int]

    it "fastFib Induction" $ property $
      \n -> (n > 1 && n < fastFibLimit) ==>
        fastFib (n :: Word) == fastFib (n - 1) + fastFib (n - 2)

    it "fastFibSum" $ property $
      \n -> (n > 1 && n < fastFibLimit - 2) ==>
        fastFibSum (n :: Word) == sum (map fastFib [1..n])

    it "approxFibLt" $ property $
      \n -> (n > 1 && n < fastFib (fastFibLimit - 1)) ==> let
          k = approxFibLt (n :: Int)
        in fastFib k <= n && fastFib (k + 1) >= n

    it "fastFibLt" $ property $
      \n -> (n > 1 && n < fastFib (fastFibLimit - 1)) ==> let
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

  describe "runProblem" $ do
    it "Fails gracefully" $ runProblem 999999 `shouldBe` "Unknown problem"
    it "Solves #1" $ runProblem 1 `shouldBe` "233168"
    it "Solves #2" $ runProblem 2 `shouldBe` "4613732"
    it "Solves #3" $ runProblem 3 `shouldBe` "6857"
    it "Solves #4" $ runProblem 4 `shouldBe` "906609"
