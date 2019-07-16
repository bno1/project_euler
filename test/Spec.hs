import Test.Hspec
import Test.QuickCheck
import Lib
import Common

main :: IO ()
main = hspec $ do
  it "Sum N" $ property $ \n -> nSum (n :: Int) == sum [1..n]
  it "Sum Mult N" $ property $
    \n lim -> (n > 0) ==>
      sumOfMults lim (n :: Word) == sum (takeWhile (<lim) [n,2*n..])
  it "FastFib" $ map fastFib [0, 1, 2] `shouldBe` [0, 1, 1]
  it "FastFib 2" $ property $
    \n -> (n > 1 && n < 76) ==>
      fastFib (n :: Word) == fastFib (n - 1) + fastFib (n - 2)
  it "FastFibSum" $ property $
    \n -> (n > 1 && n < 74) ==>
      fastFibSum (n :: Word) == sum (map fastFib [1..n])
  it "Fails gracefully" $ runProblem 999999 `shouldBe` "Unknown problem"
  it "Solves #1" $ runProblem 1 `shouldBe` "233168"
