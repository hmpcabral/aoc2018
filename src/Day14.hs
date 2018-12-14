module Day14 (part1, part2) where

import Data.List (iterate')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

type Score = Int

data Recipes = Recipes { getScores :: IntMap Score
                       , getFocus :: (Int, Int)
                       , getLength :: Int }
             deriving (Show)

initial :: Recipes
initial = Recipes { getScores = IM.fromList [(0, 3), (1, 7)]
                  , getFocus = (0, 1)
                  , getLength = 2 }

update :: Recipes -> ([Int], Recipes)
update (Recipes scores (x, y) n) = (ns, Recipes scores' (x', y') n')
  where scores' = foldr (\(i, s) m -> IM.insert i s m) scores $ zip [n..] ns
        n' = n + length ns
        (x', y') = (x `advance` sx, y `advance` sy)
        ns = let s = sx + sy
             in if s < 10 then [s] else [s `div` 10, s `mod` 10]
        (sx, sy) = (scores IM.! x, scores IM.! y)

        advance p d = (p + d + 1) `mod` n'

part1 :: Int -> [Int]
part1 n = map (getScores rs IM.!) [n..n+9]
  where rs = until ((>= n + 10) . getLength) (snd . update) initial

part2 :: [Int] -> Int
part2 needle = go needle needle $ iterate' (update . snd) ([], initial)

go :: Eq a => [a] -> [a] -> [([a], Recipes)] -> Int
go needle missing ((ns, rs):xs)
  | null ns       = go needle needle xs
  | null missing' = getLength rs - length needle - length ns + length missing
  | otherwise     = go needle missing' xs'
  where (missing', xs') = case (missing `starts` ns, needle `starts` ns) of
                              (False, False) -> (needle, ((drop 1 ns, rs):xs))
                              (True,  _)     -> (drop (length ns) missing, xs)
                              (_,     True)  -> (drop (length ns) needle, xs)

        x `starts` y = take (length y) x == take (length x) y
