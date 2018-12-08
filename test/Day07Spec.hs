module Day07Spec where

import Test.Hspec
import Test.QuickCheck
import Data.Char (ord)
import Data.List (inits, tails, sort)
import qualified Data.Map as M
import Control.Monad
import Day07

newtype TestManual = TestManual (M.Map Char [Char]) deriving (Show)

instance Arbitrary TestManual where
    arbitrary = do k <- choose (1, 26)
                   tasks <- shuffle $ take k ['A'..'Z']
                   pairs <- forM (init $ tails tasks) $ \(t:ts) -> do
                       dep <- sublistOf ts
                       return (t, dep)
                   return $ TestManual (M.fromList pairs)

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
    it "includes all tasks exactly once" $ property $
      \(TestManual m) -> sort (order m) == sort (M.keys m)

    it "meets all dependencies" $ property $
      \(TestManual m) -> let valid done task = all (`notElem` done) (m M.! task)
                             steps = init $ zip (inits tasks) (tails tasks)
                             tasks = order m
                         in all (\(done, (t:_)) -> valid done t) steps

    it "sample case" $ do
      order sampleManual `shouldBe` "CABDFE"

  describe "parallelWork" $ do
    it "sequences tasks with a single worker" $ property $
      \(TestManual m) -> parallelWork 1 (\_ -> 1) m == M.size m

    it "is monotonic over the number of workers" $ property $
      \(TestManual m) (Positive w1) (Positive w2) (Fn f) ->
          let (wmin, wmax) = (min w1 w2, max w1 w2)
              duration = getPositive . f
          in parallelWork wmin duration m >= parallelWork wmax duration m

    it "sample case" $ do
      parallelWork 2 (\c -> ord c - ord 'A' + 1) sampleManual `shouldBe` 15
