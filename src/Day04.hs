module Day04 where

import Text.ParserCombinators.ReadP
import Data.Time
import Data.Time.Format (readPTime)
import Data.Char (isDigit)
import Data.List (nub, sortOn, mapAccumL, mapAccumR, maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Array.Unboxed (UArray, listArray, accum, assocs)

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
sliceGuard :: [Event] -> Action -> Int -> [Event]
sliceGuard evs action guard = filter (\e -> fg e && fa e) evs
  where fg e = (getGuard e) == Just guard
        fa e = (getAction e )== action

totalSleep :: [Event] -> Int -> NominalDiffTime
totalSleep evs = sum . map td . sliceGuard evs FallAsleep
  where td x = diffUTCTime (getEnd x) (getStart x)

allGuards :: [Event] -> [Int]
allGuards = nub . map (fromJust . getGuard)

sleepy :: [Event] -> Int
sleepy evs = snd . maximumBy cmpDuration $ zip (map (totalSleep evs) guards) guards
  where guards = allGuards evs
        cmpDuration x y = compare (fst x) (fst y)

timeCard :: [Event] -> UArray Int Int
timeCard = foldr acc empty
  where start = todMin . timeToTimeOfDay . utctDayTime . getStart
        end = todMin . timeToTimeOfDay . utctDayTime . getEnd
        empty = listArray (0, 59) (repeat 0)
        acc ev card = accum (+) card (zip [start ev..end ev - 1] (repeat 1))

napTime :: [Event] -> Int -> (Int, Int)
napTime evs guard = maximumBy (comparing snd) . assocs . timeCard $ sliceGuard evs FallAsleep guard

sleepiestTime :: [Event] -> (Int, Int, Int)
sleepiestTime evs = maximumBy cmpThird . zipWith consTuple guards $ map (napTime evs) guards
  where guards = allGuards evs
        cmpThird (_, _, x) (_, _, y) = compare x y
        consTuple x (y, z) = (x, y, z)
