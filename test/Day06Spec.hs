module Day06Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad (liftM)
import Data.List (nub)
import Day06

makeGrid :: [Coord] -> Grid
makeGrid cs = Grid (bounds cs) cs

instance Arbitrary Grid where
  arbitrary = liftM (makeGrid . nub) $ listOf1 arbitrary

-- AoC sample
samples = readGrid [ "1, 1"
                   , "1, 6"
                   , "8, 3"
                   , "3, 4"
                   , "5, 5"
                   , "8, 9" ]

-- Helpers
boundedAreas g@(Grid _ cs) = filter ((/= Unbounded) . snd) $ zip cs areas
  where areas = map (area g) cs

-- Property checkers
isClose (Grid _ cs) (s, Bounded xs) = all (\c -> all (farFrom c) xs) other
  where farFrom c x = mdist s x < mdist c x
        other = filter (/= s) cs

-- Specification
spec = do
  describe "area" $ do
    it "is closest to the source coordinate than all others" $ property $
      \grid -> all (isClose grid) $ boundedAreas grid

  describe "empire" $ do
    it "sample case" $ do
      empire samples `shouldBe` 17

  describe "safeArea" $ do
    it "sample case" $ do
      fmap areaWithin (safeBoundary samples 32) `shouldBe` Just 16
