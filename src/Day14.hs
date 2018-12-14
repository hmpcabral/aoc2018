module Day14 (part1, part2) where

import Data.List (tails, isPrefixOf)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

type Score = Int

data Recipes = Recipes { getScores :: Seq Score
                       , getFocus :: (Int, Int) }
             deriving (Show)

update :: Recipes -> ([Score], Recipes)
update (Recipes scores (x, y)) = (ns, Recipes scores' (x', y'))
  where scores' = scores <> Seq.fromList ns
        (x', y') = (x `advance` sx, y `advance` sy)
        ns = let s = sx + sy
             in if s < 10 then [s] else [s `div` 10, s `mod` 10]
        (sx, sy) = (scores `Seq.index` x, scores `Seq.index` y)

        advance p d = (p + d + 1) `mod` length scores'

stream :: [Score]
stream = 3 : 7 : go (Recipes (Seq.fromList [3,7]) (0,1))
  where go r = let (new, r') = update r in new ++ go r'

part1 :: Int -> [Score]
part1 n = take 10 $ drop n stream

part2 :: [Score] -> Int
part2 needle = length . takeWhile (not . isPrefixOf needle) $ tails stream
