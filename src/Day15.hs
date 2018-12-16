{-# LANGUAGE RecordWildCards #-}

module Day15 where

import Data.List (minimumBy, foldl', iterate')
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import Data.Graph.Inductive (Gr)
import qualified Data.Graph.Inductive as G
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

data Class = Elf | Goblin deriving (Eq, Ord, Show)

data Unit = Unit { getClass :: Class
                 , getHP :: Int }
          deriving (Eq, Show)

data Battle = Battle { terrain :: Gr () Int
                     , units :: IntMap Unit
                     , width :: Int
                     , elfPower :: Int }
            deriving (Show)

isEnemy :: Unit -> Unit -> Bool
isEnemy u v = getClass u /= getClass v

{- READING

   1. Create a grid
   2. Remove all wall nodes
   3. Create an IntMap with all units keyed by node
-}
grid :: Int -> Int -> Gr () Int
grid w h = G.undir $ G.mkGraph nodes edges
  where nodes = [(node, ()) | node <- [0..w * h - 1]]
        edges =  [(n - w, n, 1) | (n, _) <- nodes, n >= w]
              ++ [(n - 1, n, 1) | (n, _) <- nodes, n >= 1, n `mod` w /= 0]

parseInput :: [String] -> Battle
parseInput ls = foldr go start . zip [0..] $ concat ls
  where start = Battle { terrain = grid w (length ls)
                       , units = IM.empty
                       , width = w
                       , elfPower = 3 }
        w = length (head ls)

        go (n, '#') b@Battle{..} = b {terrain = G.delNode n terrain}
        go (n, 'E') b@Battle{..} = b {units = IM.insert n (Unit Elf    200) units}
        go (n, 'G') b@Battle{..} = b {units = IM.insert n (Unit Goblin 200) units}
        go _        b            = b

{- MOVING

   1. List in-range squares
   2. If in in-range square, done
   3. Remove all other units from the graph
   4. Find the closest in-range square (in reading order)
   5. Find the shortest path lengths from each neighbouring node
   6. Pick the first node in reading order
-}
moveUnit :: Int -> Battle -> Maybe (Int, Battle)
moveUnit n b@Battle{..}
  | null enemies          = Nothing
  | IS.null targets       = Just (n,  b)
  | n `IS.member` targets = Just (n,  b)
  | otherwise             = Just (n', b {units = units'})
  where unit = units IM.! n
        targets = IS.filter (`elem` G.reachable n g') inRange
        g' = G.delNodes (IS.elems . IS.delete n $ IM.keysSet units) terrain

        inRange = IS.unions $ map (IS.fromList . G.neighbors terrain) enemies
        enemies = IM.keys $ IM.filter (isEnemy unit) units

        closest = minimumBy (comparing $ dist n) $ IS.elems targets
        dist from to = G.spLength from to g'

        options = filter (`elem` G.neighbors g' n) [n - width, n - 1, n + 1, n + width]
        n' = minimumBy (comparing $ dist closest) options
        units' = IM.insert n' unit $ IM.delete n units

{- ATTACKING

   1. List adjacent units
   2. Pick unit with fewest HP sorted by reading order
   3. Deal damage
-}
attack :: Int -> Battle -> (Maybe Int, Battle)
attack n b@Battle{..}
  | null enemies  = (Nothing,     b)
  | getHP t' <= 0 = (Just target, b {units = IM.delete target units})
  | otherwise     = (Nothing,     b {units = IM.insert target t' units})
  where unit = units IM.! n
        adjacent = filter (`IM.member` units) [n - width, n - 1, n + 1, n + width]
        enemies = filter (isEnemy unit . (units IM.!)) adjacent
        target = minimumBy (comparing $ getHP . (units IM.!)) enemies
        Unit c h = units IM.! target
        t' = Unit c (h - power)
        power = if getClass unit == Elf then elfPower else 3

doRound :: Battle -> (Bool, Battle)
doRound battle = (done', battle')
  where (done', _, battle') = foldl' go (False, [], battle) us
        us = IM.keys $ units battle
        go (done, ds, b) u
          | u `elem` ds     = (done, ds,   b)
          | Nothing <- move = (True, ds,   b)
          | Just d <- md    = (done, d:ds, b'')
          | otherwise       = (done, ds,   b'')
          where Just (u', b') = move
                move = moveUnit u b
                (md, b'') = attack u' b'

runBattle :: Battle -> (Int, Battle)
runBattle start = (length full - 1, snd x)
  where (full, (x:_)) = break fst $ iterate' (doRound . snd) (False, start)

outcome :: Int -> Battle -> Int
outcome rounds b = rounds * sum (map getHP . IM.elems $ units b)

findPowerup :: Battle -> (Int, Int)
findPowerup b = (elfPower $ snd win, uncurry outcome $ win)
  where nElves = IM.size . IM.filter ((== Elf) . getClass) . units
        (_, win:_) = break (\(_, b') -> nElves b' == nElves b) $ map runBattle powered
        powered = zipWith (\p b -> b {elfPower = p}) [4..] (repeat b)
