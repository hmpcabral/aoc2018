module Day14 (part1, part2) where

import Data.List (tails, isPrefixOf)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

type Score = Int

data Recipes = Recipes { getScores :: IntMap Score
                       , getFocus :: (Int, Int)
                       , getLength :: Int }
             deriving (Show)

update :: Recipes -> ([Score], Recipes)
update (Recipes scores (x, y) n) = (ns, Recipes scores' (x', y') n')
  where scores' = foldr (\(i, s) m -> IM.insert i s m) scores $ zip [n..] ns
        n' = n + length ns
        (x', y') = (x `advance` sx, y `advance` sy)
        ns = let s = sx + sy
             in if s < 10 then [s] else [s `div` 10, s `mod` 10]
        (sx, sy) = (scores IM.! x, scores IM.! y)

        advance p d = (p + d + 1) `mod` n'

stream :: [Score]
stream = 3 : 7 : go (Recipes (IM.fromList [(0,3), (1,7)]) (0,1) 2)
  where go r = let (new, r') = update r in new ++ go r'

part1 :: Int -> [Score]
part1 n = take 10 $ drop n stream

part2 :: [Score] -> Int
part2 needle = length . takeWhile (not . isPrefixOf needle) $ tails stream
