module Day11 where

import Data.List (foldl')
import Data.Array.Unboxed (UArray, (!))
import qualified Data.Array.Unboxed as U

type Coord = (Int, Int)
type Power = Int
type Grid = UArray Coord Power

squareUpto :: Coord -> Int -> [Coord]
squareUpto (x, y) u = U.range ((x, y), (x + u - 1, y + u - 1))

fillCells :: Int -> Grid
fillCells serial = U.array bounds [(c, power c) | c <- squareUpto (1, 1) 300]
    where power (x, y) = (x + 10) * ((x + 10) * y + serial) `div` 100 `mod` 10 - 5
          bounds = ((1, 1), (300, 300))

maxSquareOf :: Int -> Int -> (Power, Coord)
maxSquareOf serial sz = maximum [(at c, c) | c <- squareUpto (1,1) (300-sz+1)]
  where grid = fillCells serial
        at c = sum . map (grid !) $ squareUpto c sz

maxSquare :: Int -> (Coord, Int)
maxSquare serial = let (_, _, (_, c, s)) = foldl' go start [1..299] in (c, s)
  where grid = fillCells serial
        empty = U.listArray (U.bounds grid) $ repeat 0
        start = (grid, empty, (0, (0, 0), 0))

        go :: (Grid, Grid, (Power, Coord, Int)) -> Int -> (Grid, Grid, (Power, Coord, Int))
        go (memo', memo, maxPower) sz = (memo'', memo', max maxPower maxPower')
          where maxPower' = addSize $ maximum [(memo'' ! c, c) | c <- cells]
                addSize (a, b) = (a, b, sz + 1)

                memo'' = U.accum (+) memo' [(c, grow c) | c <- cells]
                cells = squareUpto (1, 1) (300 - sz)
                grow (x, y) = sum $ [ memo' ! (x + 1, y + 1)
                                    , grid ! (x + sz, y)
                                    , grid ! (x, y + sz)
                                    , negate $ memo ! (x + 1, y + 1) ]
