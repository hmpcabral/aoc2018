module Day04Spec (spec) where

import Test.Hspec
import Day04

-- Original sample from AoC
samples = readEvents [ "[1518-11-01 00:00] Guard #10 begins shift"
                     , "[1518-11-01 00:05] falls asleep"
                     , "[1518-11-01 00:25] wakes up"
                     , "[1518-11-01 00:30] falls asleep"
                     , "[1518-11-01 00:55] wakes up"
                     , "[1518-11-01 23:58] Guard #99 begins shift"
                     , "[1518-11-02 00:40] falls asleep"
                     , "[1518-11-02 00:50] wakes up"
                     , "[1518-11-03 00:05] Guard #10 begins shift"
                     , "[1518-11-03 00:24] falls asleep"
                     , "[1518-11-03 00:29] wakes up"
                     , "[1518-11-04 00:02] Guard #99 begins shift"
                     , "[1518-11-04 00:36] falls asleep"
                     , "[1518-11-04 00:46] wakes up"
                     , "[1518-11-05 00:03] Guard #99 begins shift"
                     , "[1518-11-05 00:45] falls asleep"
                     , "[1518-11-05 00:55] wakes up" ]

-- Specification
spec = do
  describe "part1" $ do
    it "sample case" $ do
      part1 samples `shouldBe` (10, 24)

  describe "part2" $ do
    it "sample case" $ do
      part2 samples `shouldBe` (99, 45)
