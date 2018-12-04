module Day04 where

import Text.ParserCombinators.ReadP
import Data.Time
import Data.Time.Format (readPTime)
import Data.Char (isDigit)
import Data.List (nub, sortOn, mapAccumL, mapAccumR, maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Array.Unboxed (UArray, listArray, accum, assocs, elems)
import Data.Array.ST
import qualified Data.Map as M (Map, empty, insert, insertWith, foldrWithKey, keys, (!))
import Control.Monad

data Action = StartShift | FallAsleep | WakeUp
            deriving (Eq, Show)

data Event = Event { getGuard :: Maybe Int
                   , getAction :: Action
                   , getStart :: UTCTime
                   , getEnd :: UTCTime }
           deriving (Eq, Show)

--
-- Parsing
--
number :: ReadP Int
number = fmap read $ many1 (satisfy isDigit)

timeP :: ReadP UTCTime
timeP = readPTime False defaultTimeLocale "%Y-%-m-%-d %H:%M"

eventP :: ReadP Event
eventP = do
    time <- between (char '[') (char ']') timeP
    skipSpaces
    (action, guard) <- choice [fallAsleepP, wakeUpP, startShiftP]
    return (Event guard action time undefined)
  where fallAsleepP = string "falls asleep" >> return (FallAsleep, Nothing)
        wakeUpP     = string "wakes up" >> return (WakeUp, Nothing)
        startShiftP = do
          guard <- between (string "Guard #") (string " begins shift") number
          return (StartShift, Just guard)

readEvent :: String -> Event
readEvent str = let ((event, _):_) = readP_to_S eventP str in event

readEvents :: [String] -> [Event]
readEvents = fillEnds . fillGuards . sortOn getStart . map readEvent
  where fillGuards = snd . mapAccumL fillGuard Nothing
        fillGuard last e@(Event g _ _ _) = case g of
                                             Nothing -> (last, e {getGuard = last})
                                             _       -> (g, e)

        fillEnds xs = snd $ mapAccumR fillEnd (last xs) (init xs)
        fillEnd next@(Event _ _ s _) prev = (f, f)
          where f = prev {getEnd = s}

--
-- Solve
--
type PunchedClock = UArray Int Int
type TimeCard = M.Map Int PunchedClock

guardClock :: [Event] -> Int -> PunchedClock
guardClock evs guard = runSTUArray $ do
    clock <- newArray (0, 59) 0
    forM_ evs $ \(Event g action start end) -> do
        when (g == Just guard && action == FallAsleep) $ do
            forM_ [minutes start..minutes end - 1] $ \m -> do
                count <- readArray clock m
                writeArray clock m (count + 1)
    return clock
  where minutes = todMin . timeToTimeOfDay . utctDayTime

allGuards :: [Event] -> [Int]
allGuards = nub . map (fromJust . getGuard)

timecard :: [Event] -> TimeCard
timecard evs = foldr (\g c -> M.insert g (guardClock evs g) c) M.empty $ allGuards evs

sleepy :: TimeCard -> Int
sleepy = fst . M.foldrWithKey maxSleep (0, 0)
  where maxSleep guard clock cur@(maxGuard, maxTime) =
          let sleep = sum $ elems clock in if sleep > maxTime then (guard, sleep) else cur

napTime :: TimeCard -> Int -> (Int, Int)
napTime card = maximumBy (comparing snd) . assocs . (card M.!)

sleepiestTime :: TimeCard -> (Int, Int, Int)
sleepiestTime card = maximumBy (comparing (\(g, m, c) -> c)) naps
  where naps = map (\g -> let (m, c) = napTime card g in (g, m, c)) guards
        guards = M.keys card
