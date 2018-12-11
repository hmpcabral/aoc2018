module Day11 where

import Data.List (foldl')
import Data.Array.Unboxed (UArray, (!))
import qualified Data.Array.Unboxed as U
import qualified Data.Array as A

type Coord = (Int, Int)
type Power = Int

squareUpto :: Coord -> Int -> [Coord]
squareUpto (x, y) u = U.range ((x, y), (x + u - 1, y + u - 1))

fillCells :: Int -> UArray Coord Power
fillCells serial = U.array bounds [(c, power c) | c <- squareUpto (1, 1) 300]
    where power (x, y) = (x + 10) * ((x + 10) * y + serial) `div` 100 `mod` 10 - 5
          bounds = ((1, 1), (300, 300))

maxSquareOf :: Int -> Int -> (Power, Coord)
maxSquareOf serial s = maximum [ (at c, c)
                               | c <- U.range ((1,1), (300-s+1,300-s+1)) ]
  where grid = fillCells serial
        at c = sum . map (grid !) $ squareUpto c s

combine :: A.Ix a => (a, a) -> [(a, a -> b)] -> [b]
combine bs xs = map (\(p, f) -> f p) $ filter ((A.inRange bs) . fst) xs

maxSquare :: Int -> (Coord, Int)
maxSquare serial = snd $ maximum [ (at s c, (c, s))
                                   | s <- [1..300]
                                   , c <- U.range ((1,1), (300-s+1, 300-s+1)) ]
  where ps = partials (fillCells serial)
        at s (x, y) = sum . combine (A.bounds ps) $
                          [ ((x + s - 1, y + s - 1), (ps A.!))
                          , ((x - 1,     y + s - 1), negate . (ps A.!))
                          , ((x + s - 1, y - 1),     negate . (ps A.!))
                          , ((x - 1,     y - 1),     (ps A.!)) ]

partials :: UArray Coord Power -> A.Array Coord Power
partials grid = ps
  where ps = A.listArray (U.bounds grid) . map partial $ U.indices grid
        partial (x, y) = sum . combine (U.bounds grid) $
                             [ ((x,     y),     (grid !))
                             , ((x - 1, y),     (ps A.!))
                             , ((x,     y - 1), (ps A.!))
                             , ((x - 1, y - 1), negate . (ps A.!)) ]
