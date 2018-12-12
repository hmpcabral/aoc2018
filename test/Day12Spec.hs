module Day12Spec where

import Test.Hspec
import Day12

-- AoC sample
(aocRules, aocGarden) = parseInput
                      . unlines $ [ "initial state: #..#.#..##......###...###"
                                  , ""
                                  , "..#.. => #"
                                  , "#...# => ."
                                  , ".#... => #"
                                  , "#.##. => ."
                                  , "..#.# => ."
                                  , "#.#.# => #"
                                  , "###.. => #"
                                  , "###.# => #"
                                  , "..... => ."
                                  , "....# => ."
                                  , ".##.. => #"
                                  , "##### => ."
                                  , "####. => #"
                                  , "..##. => ."
                                  , "##.#. => #"
                                  , ".#..# => ."
                                  , "##..# => ."
                                  , ".##.# => ."
                                  , ".#### => #"
                                  , "..### => ."
                                  , "...## => #"
                                  , "#..## => ."
                                  , "#.... => ."
                                  , "##.## => #"
                                  , "#.#.. => ."
                                  , "##... => ."
                                  , ".#.## => #"
                                  , ".###. => ."
                                  , "...#. => ."
                                  , "#.### => #"
                                  , "#..#. => ."
                                  , ".#.#. => #" ]

-- Specification
spec = do
  describe "aliveAt" $ do
    it "handles small cases" $ do
      aliveAt aocRules aocGarden 20 `shouldBe` 325

    it "handles large cases" $ do
      aliveAt aocRules aocGarden 10000000000 `shouldBe` 199999999374
