module Day08Spec where

import Test.Hspec
import Test.QuickCheck
import Day08

newtype NoChildren = NoChildren Tree deriving (Show)

arbitraryDepth :: Int -> Gen Tree
arbitraryDepth depth = do
   nc <- choose (0, if depth /= 0 then 5 else 0)
   meta <- listOf1 (fmap getNonNegative arbitrary)
   if nc == 0 then return $ Tree [] meta
              else do children <- vectorOf nc $ arbitraryDepth (depth - 1)
                      return $ Tree children meta

instance Arbitrary Tree where
  arbitrary = do depth <- choose (0, 5)
                 arbitraryDepth depth

instance Arbitrary NoChildren where
  arbitrary = fmap NoChildren $ arbitraryDepth 0

-- AoC sample
sampleTree = readTree "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

-- Specification
spec = do
  describe "addMetadata" $ do
    it "sample case" $ do
      addMetadata sampleTree `shouldBe` 138

  describe "value" $ do
    it "sample case" $ do
      value sampleTree `shouldBe` 66

  describe "both metrics" $ do
    it "match for childless trees" $ property $
      \(NoChildren t) -> addMetadata t == value t
