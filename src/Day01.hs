module Day01 (toInts, finalFreq, firstRep) where

import Data.Char
import qualified Data.Set as Set

toInts :: String -> [Int]
toInts str = map (read . dropWhile (== '+')) $ lines str

finalFreq :: [Int] -> Int
finalFreq = sum

dup :: Ord a => Set.Set a -> [a] -> a
dup s (x:xs) = if x `Set.member` s then x else dup (x `Set.insert` s) xs

firstRep :: [Int] -> Int
firstRep deltas = dup Set.empty . scanl (+) 0 $ cycle deltas
