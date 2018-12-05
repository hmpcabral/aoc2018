module Day04 where

import Text.ParserCombinators.ReadP
import Data.Time
import Data.Time.Format (readPTime)
import Data.Char (isDigit)
import Data.List (nub, sortOn, mapAccumL, mapAccumR, maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Array.Unboxed (UArray, listArray, accum, assocs, elems)
import qualified Data.Map as M (Map, empty, insertWith, foldrWithKey, keys, (!))

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

punchClock :: PunchedClock -> Event -> PunchedClock
punchClock clock (Event _ _ start end) = accum (\x _ -> x+1) clock entries
  where entries = zip [minutes start..minutes end - 1] [1..]
        minutes = todMin . timeToTimeOfDay . utctDayTime

timecardInsert :: Event -> TimeCard -> TimeCard
timecardInsert ev = M.insertWith joinClocks (fromJust $ getGuard ev) (newClock ev)
  where joinClocks x y = accum (+) x (assocs y)
        newClock = punchClock emptyClock
        emptyClock = listArray (0, 59) (repeat 0)

timecard :: [Event] -> TimeCard
timecard = foldr timecardInsert M.empty . sleeps
  where sleeps = filter ((== FallAsleep) . getAction)

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
