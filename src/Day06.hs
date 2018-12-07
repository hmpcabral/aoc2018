module Day06 where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.Set (Set, insert, member, empty, toList)
import Control.Monad (forM_)

type Coord = (Int, Int)
type Bounds = (Coord, Coord)

data Grid = Grid Bounds [Coord] deriving (Show)

-- Parsing
coordP :: ReadP Coord
coordP = do
    x <- number
    string ", "
    y <- number
    eof
    return (read x, read y)
  where number = many1 (satisfy isDigit)

readCoord :: String -> Coord
readCoord str = let ((coord, _):_) = readP_to_S coordP str in coord

readGrid :: [String] -> Grid
readGrid ls = Grid (bounds cs) cs
  where cs = map readCoord ls

-- Common
mdist :: Coord -> Coord -> Int
mdist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

neighbours :: Coord -> [Coord]
neighbours (x, y) = [ (x - 1, y - 1)
                    , (x - 1, y)
                    , (x - 1, y + 1)
                    , (x,     y - 1)
                    , (x,     y + 1)
                    , (x + 1, y - 1)
                    , (x + 1, y)
                    , (x + 1, y + 1) ]

bounds :: [Coord] -> Bounds
bounds xs = ((minX, minY), (maxX, maxY))
  where minX = minimum $ map fst xs
        minY = minimum $ map snd xs
        maxX = maximum $ map fst xs
        maxY = maximum $ map snd xs

inBounds :: Bounds -> Coord -> Bool
inBounds ((lx, ly), (mx, my)) (x, y) = lx <= x && x <= mx && ly <= y && y <= my

-- Part 1
data Area = Bounded [Coord] | Unbounded deriving (Eq, Show)

grow :: Grid -> Coord -> Coord -> Set Coord -> Set Coord
grow grid@(Grid bs xs) source coord path
  | not (inBounds bs coord) = path
  | coord `member` path     = path
  | any closer xs           = path
  | otherwise               = foldr (grow grid source) path' $ neighbours coord
  where path' = insert coord path
        closer x = x /= source && mdist coord x <= mdist coord source

atEdge :: Bounds -> Coord -> Bool
atEdge ((lx, ly), (mx, my)) (x, y) = x == lx || x == mx || y == ly || y == my

area :: Grid -> Coord -> Area
area grid@(Grid bs _) x
  | any (atEdge bs) xs = Unbounded
  | otherwise          = Bounded xs
  where xs = toList $ grow grid x x empty

empire :: Grid -> Int
empire grid@(Grid _ xs) = maximum . map areaSize $ map (area grid) xs
  where areaSize (Bounded xs) = length xs
        areaSize Unbounded = 0

-- Part 2
data Boundary = Boundary { bTop :: [Coord]
                         , bBottom :: [Coord]
                         , bLeft :: [Coord]
                         , bRight :: [Coord] }
              deriving (Show, Eq)

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x = fst $ until (\(a, b) -> a == b) (\(_, b) -> (b, f b)) (x, f x)

shrinkOne :: Grid -> Int -> Maybe Boundary -> Maybe Boundary
shrinkOne _ _ Nothing = Nothing
shrinkOne (Grid _ xs) dist (Just (Boundary top bottom left right))
  | any null [st, sb, sl, sr] = Nothing
  | otherwise                 = Just (Boundary st sb sl sr)
  where mt = map (move (\(x, y) -> (x, y - 1))) top
        mb = map (move (\(x, y) -> (x, y + 1))) bottom
        ml = map (move (\(x, y) -> (x + 1, y))) left
        mr = map (move (\(x, y) -> (x - 1, y))) right

        xb = bounds (ml ++ mr)
        yb = bounds (mt ++ mb)

        [st, sb] = map (filter (inBounds xb)) [mt, mb]
        [sl, sr] = map (filter (inBounds yb)) [ml, mr]

        move f p = if reached p then p else f p
        reached p = totalDist p < dist
        totalDist x = sum $ map (mdist x) xs

shrink :: Grid -> Int -> Boundary -> Maybe Boundary
shrink grid dist boundary = fixedPoint (shrinkOne grid dist) (Just boundary)

boundaryAt :: Grid -> Int -> Boundary
boundaryAt (Grid ((lx, ly), (mx, my)) _) dist = Boundary t b l r
  where t = [(x, my) | x <- [lx..mx]]
        b = [(x, ly) | x <- [lx..mx]]
        l = [(lx, y) | y <- [ly..my]]
        r = [(mx, y) | y <- [ly..my]]

areaWithin :: Boundary -> Int
areaWithin (Boundary top bottom _ _) = sum $ zipWith vertical top bottom
  where vertical (_, y1) (_, y2) = abs (y1 - y2) + 1

safeBoundary :: Grid -> Int -> Maybe Boundary
safeBoundary grid dist = shrink grid dist $ boundaryAt grid dist

-- Debugging aids
printBoundary :: Grid -> Boundary -> IO ()
printBoundary grid@(Grid _ coords) (Boundary t b l r) = do
    forM_ [miny..maxy] $ \y -> do
        forM_ [minx..maxx] $ \x ->
            if (x, y) `elem` coords then putChar 'X'
            else if (x, y) `elem` total then putChar 'B'
            else putChar '.'

        putChar '\n'
  where ((minx, miny), (maxx, maxy)) = bounds (total ++ coords)
        total = t ++ b ++ l ++ r

showShrinking :: Grid -> Int -> IO ()
showShrinking grid dist = do
    forM_ (iterate (shrinkOne grid dist) (Just $ boundaryAt grid dist)) $ \(Just b) -> do
        printBoundary grid b
        getChar
