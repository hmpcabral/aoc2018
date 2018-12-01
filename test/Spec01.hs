import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "toInts" $ do
    it "empty string" $ do
      toInts "" `shouldBe` []

    it "single number" $ do
      toInts "2" `shouldBe` [2]

    it "large number" $ do
      toInts "21237956" `shouldBe` [21237956]

    it "multiple non-negative numbers" $ do
      toInts "1\n52\n23\n2" `shouldBe` [1, 52, 23, 2]

    it "negative numbers" $ do
      toInts "-2\n0\n-42\n20" `shouldBe` [-2, 0, -42, 20]

    it "numbers with plus signs" $ do
      toInts "+2\n5\n-2\n+90" `shouldBe` [2, 5, -2, 90]

  describe "finalFreq" $ do
    it "short cases" $ do
      finalFreq [1, -2, 3, 1] `shouldBe` 3
      finalFreq [1, 1, 1] `shouldBe` 3
      finalFreq [1, 1, -2] `shouldBe` 0
      finalFreq [-1, -2, -3] `shouldBe` -6

  describe "firstRep" $ do
    it "short cases" $ do
      firstRep [1, -2, 3, 1] `shouldBe` 2
      firstRep [1, -1] `shouldBe` 0
      firstRep [3, 3, 4, -2, -4] `shouldBe` 10
      firstRep [-6, 3, 8, 5, -6] `shouldBe` 5
      firstRep [7, 7, -2, -7, -4] `shouldBe` 14
