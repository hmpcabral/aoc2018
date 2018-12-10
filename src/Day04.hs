module Day04 where

import Data.Text (pack, unpack, split)
import Data.List (foldl', sort, maximumBy)
import Data.Ord (comparing)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

data TimeStamp = TimeStamp { year :: Int
                           , month :: Int
                           , day :: Int
                           , hour :: Int
                           , minute :: Int }
               deriving (Eq, Show)

data Action = StartShift Int
            | FallAsleep TimeStamp
            | WakeUp TimeStamp
            deriving (Eq, Show)

-- Parsing
parseAction :: String -> Action
parseAction str = action
  where (timestamp, (a:g:_)) = splitAt 5 $ components str
        [year, mon, day, h, m] = map read timestamp
        ts = TimeStamp year mon day h m
        action = case a of
                     "Guard" -> StartShift (read g)
                     "falls" -> FallAsleep ts
                     "wakes" -> WakeUp ts

        components = filter (not . null) . map unpack . split noise . pack
        noise = (`elem` "[-:]# ")

readEvents :: [String] -> IntMap (Vector Int)
readEvents ls = card
  where (card, _, _) = foldl' event (IM.empty, Nothing, Nothing) $ readActions ls

        readActions = map parseAction . sort

        event (c,      _,          _) (StartShift g')  = (c,  Just g', Nothing)
        event (c,      g,          _) (FallAsleep ts)  = (c,       g,  Just ts)
        event (c, Just g, Just start) (WakeUp     end) = (c', Just g,  Nothing)
          where c' = IM.insertWith (V.zipWith (+)) g ms c
                ms = fill (V.replicate 60 0) (minute start) (minute end - 1)
                fill v from to = v V.// [(i, 1) | i <- [from..to]]

-- Part 1
part1 :: IntMap (Vector Int) -> (Int, Int)
part1 card = (g, V.maxIndex ts)
  where (g, ts) = maximumBy (comparing (sum . V.toList . snd)) $ IM.assocs card

-- Part 2
part2 :: IntMap (Vector Int) -> (Int, Int)
part2 card = (g, V.maxIndex ts)
   where (g, ts) = maximumBy (comparing (V.maximum . snd)) $ IM.assocs card
