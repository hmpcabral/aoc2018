module Day11Spec where

import Test.Hspec
import Day11

-- Specification
spec = do
  describe "maxSquareOf" $ do
    it "sample cases" $ do
      maxSquareOf 18 3 `shouldBe` (29, (33, 45))
      maxSquareOf 42 3 `shouldBe` (30, (21, 61))

  describe "maxSquare" $ do
    it "sample cases" $ do
      maxSquare 18 `shouldBe` ((90, 269), 16)
      maxSquare 42 `shouldBe` ((232, 251), 12)
