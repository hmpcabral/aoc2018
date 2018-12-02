module Day02Spec (spec) where

import Data.List (sort)
import Test.Hspec
import Day02

checksumIDs = [ "abcdef"
              , "bababc"
              , "abbcde"
              , "abcccd"
              , "aabcdd"
              , "abcdee"
              , "ababab" ]

candidateIDs = [ "abcde"
               , "fghij"
               , "klmno"
               , "pqrst"
               , "fguij"
               , "axcye"
               , "wvxyz" ]

spec = do
  describe "countRepetitions" $ do
    it "sample cases" $ do
      countRepetitions 2 checksumIDs `shouldBe` 4
      countRepetitions 3 checksumIDs `shouldBe` 3

  describe "checksum" $ do
    it "sample cases" $ do
      checksum checksumIDs `shouldBe` 12

  describe "findDistance" $ do
    it "sample cases" $ do
      sort (findDistance 1 candidateIDs) `shouldBe` ["fghij", "fguij"]

  describe "commonLetters" $ do
    it "differ at the start" $ do
      commonLetters "xanana" "banana" `shouldBe` "anana"

    it "differ at the end" $ do
      commonLetters "banana" "bananx" `shouldBe` "banan"

    it "multiple differences" $ do
      commonLetters "banana" "btenna" `shouldBe` "bna"

    it "no common letters" $ do
      commonLetters "banana" "ananab" `shouldBe` ""
