module Day12 (parseInput, aliveAt) where

import Data.List ((!!), elemIndex)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as U
import Data.Array.ST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Arrow ((&&&))
import Control.Monad (forM_)

type Pot = Bool
type Garden = UArray Int Pot
type Rules = Map [Pot] Pot

-- Parsing
toPots :: String -> [Pot]
toPots = map (== '#')

readState :: String -> Garden
readState str = U.listArray (-5, length ps + 4) $ (pad ++ toPots ps ++ pad)
  where ps = (words str) !! 2
        pad = replicate 5 False

readRule :: String -> ([Pot], Pot)
readRule str = (toPots start, head $ toPots result)
  where (start, result) = ((!! 0) &&& (!! 2)) $ words str

readRules :: [String] -> Rules
readRules = M.fromList . map readRule

parseInput :: String -> (Rules, Garden)
parseInput str = (readRules xs, readState x)
    where (x:_:xs) = lines str

-- Solving
nextGen :: Rules -> Garden -> Garden
nextGen rules garden = runSTUArray $ do
    garden' <- newArray (left - 5, right + 5) False
    forM_ (U.range (left - 2, right + 2)) $ \pos -> do
        writeArray garden' pos $ newPot pos
    return garden'
  where (left, right) = (head &&& last) . filter (garden U.!) $ U.indices garden
        newPot pos = (rules M.!) $ map ((garden U.!) . (pos+)) [-2..2]

generations :: Rules -> Garden -> [Garden]
generations rules = iterate (nextGen rules)

findFixed :: Rules -> Garden -> Int
findFixed rules garden = fixAt
  where Just fixAt = elemIndex True $ zipWith (==) (tail gs) gs
        gs = map U.elems $ generations rules garden

aliveAt :: Rules -> Garden -> Int -> Int
aliveAt rules garden time
  | time <= fix = aliveAt' time
  | otherwise   = (aliveAt' fix) + delta * (time - fix)
  where delta = (!! fix) $ zipWith (-) (tail counts) counts
        fix = findFixed rules garden
        counts = map alive $ generations rules garden
        aliveAt' t = alive . (!! t) $ generations rules garden
        alive = sum . map fst . filter snd . U.assocs
