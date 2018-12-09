module Day09 where

import Data.Maybe (fromJust)
import Data.List (foldl')
import Data.List.PointedList.Circular (PointedList(..))
import qualified Data.List.PointedList.Circular as PL
import qualified Data.Vector.Unboxed as V

type Marble = Int
type Circle = PointedList Marble

place :: Circle -> Marble -> (Circle, Int)
place circle m
  | m `mod` 23 /= 0 = (PL.insertLeft m $ PL.moveN 2 circle, 0)
  | otherwise       = (fromJust $ PL.delete circle', m + _focus circle')
  where circle' = PL.moveN (-7) circle

play :: Int -> Int -> (Circle, V.Vector Int)
play players marbles = foldl' acc (PL.singleton 0, V.replicate players 0) plays
  where plays = zip [1..marbles] (cycle [1..players])

        acc :: (Circle, V.Vector Int) -> (Marble, Int) -> (Circle, V.Vector Int)
        acc (marbles, score) (next, player) = (marbles', score')
          where (marbles', s) = place marbles next
                score' = V.accum (+) score [(player - 1, s)]

highScore :: Int -> Int -> Int
highScore players marbles = V.maximum . snd $ play players marbles
