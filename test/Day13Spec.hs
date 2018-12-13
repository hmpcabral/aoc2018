module Day13Spec where

import Test.Hspec
import Linear (V2(..))
import Day13

-- AoC sample part 1
aocPart1 = [ "/->-\\          "
           , "|   |  /----\\  "
           , "| /-+--+-\\  |  "
           , "| | |  | v  |   "
           , "\\-+-/  \\-+--/ "
           , "  \\------/     " ]

aocPart2 = [ "/>-<\\  "
           , "|   |   "
           , "| /<+-\\"
           , "| | | v "
           , "\\>+</ |"
           , "  |   ^ "
           , "  \\<->/" ]

-- Specification
spec = do
  describe "firstCollision" $ do
    it "sample case" $ do
      (uncurry firstCollision) (parseInput aocPart1) `shouldBe` V2 3 7

  describe "survivor" $ do
    it "sample case" $ do
      (uncurry survivor) (parseInput aocPart2) `shouldBe` V2 4 6
