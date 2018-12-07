module Day07Spec where

import Test.Hspec
import Data.Char (ord)
import Day07

-- AoC sample
--
--   -->A--->B--
--  /    \      \
-- C      -->D----->E
--  \           /
--   ---->F-----
sampleManual = readManual $ [ "Step C must be finished before step A can begin."
                            , "Step C must be finished before step F can begin."
                            , "Step A must be finished before step B can begin."
                            , "Step A must be finished before step D can begin."
                            , "Step B must be finished before step E can begin."
                            , "Step D must be finished before step E can begin."
                            , "Step F must be finished before step E can begin." ]

-- Specification
spec = do
  describe "order" $ do
    it "sample case" $ do
      order sampleManual `shouldBe` "CABDFE"

  describe "parallelWork" $ do
    it "sample case" $ do
      parallelWork 2 (\c -> ord c - ord 'A' + 1) sampleManual `shouldBe` 15
