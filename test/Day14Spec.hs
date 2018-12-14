module Day14Spec where

import Test.Hspec
import Day14

-- Specification
spec = do
  describe "part1" $ do
    it "handles small cases" $ do
      part1 9    `shouldBe` [5,1,5,8,9,1,6,7,7,9]
      part1 5    `shouldBe` [0,1,2,4,5,1,5,8,9,1]
      part1 18   `shouldBe` [9,2,5,1,0,7,1,0,8,5]
      part1 2018 `shouldBe` [5,9,4,1,4,2,9,8,8,2]

  describe "part2" $ do
    it "handles small cases" $ do
      part2 [5,1,5,8,9] `shouldBe` 9
      part2 [0,1,2,4,5] `shouldBe` 5
      part2 [9,2,5,1,0] `shouldBe` 18
      part2 [5,9,4,1,4] `shouldBe` 2018
