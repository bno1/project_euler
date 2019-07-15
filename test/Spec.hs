import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  it "Fails gracefully" $ runProblem 999999 `shouldBe` "Unknown problem"
  it "Solves #1" $ runProblem 1 `shouldBe` "233168"
