module Day09Spec where

import Test.Hspec
import Day09

-- Specification
spec =
  describe "highScore" $ do
    it "sample cases" $ do
      highScore 9    25 `shouldBe` 32
      highScore 10 1618 `shouldBe` 8317
      highScore 13 7999 `shouldBe` 146373
      highScore 17 1104 `shouldBe` 2764
      highScore 21 6111 `shouldBe` 54718
      highScore 30 5807 `shouldBe` 37305
