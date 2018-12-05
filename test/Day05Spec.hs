module Day05Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List (tails)
import Day05

newtype TestPolymer = TestPolymer String deriving (Show)

instance Arbitrary TestPolymer where
  arbitrary = fmap TestPolymer . listOf $ elements (['a'..'z'] ++ ['A'..'Z'])

pairs :: Polymer -> [[Unit]]
pairs = filter ((== 2) . length) . map (take 2) . tails

spec = do
  describe "react" $ do
    it "preserves empty string" $ do
      react "" `shouldBe` ""

    it "ignores matching polarities" $ do
      react "aacBB" `shouldBe` "aacBB"

    it "sample case" $ do
      react "dabAcCaCBAcCcaDA" `shouldBe` "dabCBAcaDA"

    it "reduces or maintains length of original polymer" $ property $
      \(TestPolymer p) -> length p >= length (react p)

    it "does not contain opposite units" $ property $
      \(TestPolymer p) -> all (\[u, v] -> not (u `opposite` v)) . pairs $ react p

    it "is idempotent" $ property $
      \(TestPolymer p) -> react p == react (react p)

  describe "optimise" $ do
    it "reduces or maintains length of original polymer" $ property $
      \(TestPolymer p) -> optimise p <= length p

    it "reduces or maintains length of original reaction" $ property $
      \(TestPolymer p) -> optimise p <= length (react p)

    it "reacting beforehand is a no-op" $ property $
      \(TestPolymer p) -> optimise p == optimise (react p)

    it "sample case" $ do
      optimise "dabAcCaCBAcCcaDA" `shouldBe` 4
