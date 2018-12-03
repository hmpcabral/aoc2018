module Day03Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Printf
import Control.Monad (liftM)
import Data.Array.Unboxed (UArray, listArray, elems, accum, (!))
import Day03

instance Arbitrary Claim where
  arbitrary = liftM (\[cid, x, y, w, h] -> Claim cid x y w h) vals
    where vals = liftM (map getNonNegative) (vector 5) `suchThat` all (<50)

makeSeq :: [Claim] -> [Claim]
makeSeq = zipWith (\i c -> c {claimId = i}) [1..]

showClaim :: Claim -> String
showClaim (Claim cid x y w h) = printf "#%u @ %u,%u: %ux%u" cid x y w h

-- Alternative array-based implementation for property testing
--
-- This is much slower than the actual solution, but it's easier to get right.
type Fabric = UArray (Int, Int) Int

emptyFabric :: Int -> Int -> Fabric
emptyFabric w h = listArray ((0, 0), (w, h)) (repeat 0)

squares :: Claim -> [(Int, Int)]
squares (Claim _ cx cy w h) = [(x, y) | x <- [cx..cx+w-1], y <- [cy..cy+h-1]]

markClaim :: Claim -> Fabric -> Fabric
markClaim c f = accum (+) f $ zip (squares c) (repeat 1)

markClaims :: [Claim] -> Fabric
markClaims [] = emptyFabric 1 1 -- arbitrarily use a 1x1 grid
markClaims cs = foldr markClaim (emptyFabric w h) cs
  where w = maximum $ map (\c -> (claimX c) + (width c)) cs
        h = maximum $ map (\c -> (claimY c) + (height c)) cs

altCountOverlap :: [Claim] -> Int
altCountOverlap = length . filter (>1) . elems . markClaims

hasOverlap :: Fabric -> Claim -> Bool
hasOverlap f = any (\s -> (f ! s) > 1) . squares

altNonOverlapping :: [Claim] -> [Claim]
altNonOverlapping cs = filter (not . hasOverlap fabric) cs
  where fabric = markClaims cs

-- Original samples from AoC
samples = map readClaim [ "#1 @ 1,3: 4x4"
                        , "#2 @ 3,1: 4x4"
                        , "#3 @ 5,5: 2x2" ]

-- Specification
spec = do
  describe "readClaim" $ do
    it "reads correctly formatted claims" $ property $
      \c -> (readClaim . showClaim) c == c

  describe "countOverlap" $ do
    it "no overlap without claims" $ do
      countOverlap [] `shouldBe` 0

    it "no overlap with single claim" $ do
      countOverlap (take 1 samples) `shouldBe` 0

    it "sample cases" $ do
      countOverlap samples `shouldBe` 4

    it "array-based implementation" $ property $
      \cs -> let ucs = makeSeq cs in countOverlap ucs == altCountOverlap ucs

  describe "nonOverlapping" $ do
    it "no overlap without claims" $ do
      nonOverlapping [] `shouldBe` []

    it "single claim is nonoverlapping" $ do
      nonOverlapping (take 1 samples) `shouldBe` (take 1 samples)

    it "sample cases" $ do
      nonOverlapping samples `shouldBe` [last samples]

    it "array-based implementation" $ property $
      \cs -> let ucs = makeSeq cs in nonOverlapping ucs == altNonOverlapping ucs
