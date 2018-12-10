module Day10 (readLights, message) where

import Data.Text (pack, unpack, split)
import Data.List ((!!))
import Control.Monad (join)
import Control.Arrow ((***), (&&&), second)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

type Pos = (Int, Int)
type Vel = (Int, Int)

-- Parsing
readLight :: String -> (Pos, Vel)
readLight str = join (***) (tuple . (components !!)) (1, 3)
  where components = map unpack . split (`elem` "<>") $ pack str
        tuple s = read $ "(" ++ s ++ ")"

readLights :: [String] -> (Vector Pos, Vector Vel)
readLights = V.unzip . V.fromList . map readLight

-- Solving
forward :: Vector Vel -> Vector Pos -> Vector Pos
forward = V.zipWith (\(dx, dy) (x, y) -> (x + dx, y + dy))

rangeMap :: (Ord a, V.Unbox b, V.Unbox a) => (b -> a) -> Vector b -> (a, a)
rangeMap f = (V.minimum &&& V.maximum) . V.map f

showMessage :: Vector Pos -> [String]
showMessage ps = [[plot (x, y) | x <- [xmin..xmax]] | y <- [ymin..ymax]]
  where ((xmin, xmax), (ymin, ymax)) = (rangeMap fst &&& rangeMap snd) ps
        plot p = if p `V.elem` ps then '#' else '.'

message :: Vector Pos -> Vector Vel -> (Int, [String])
message pos vel = second showMessage $ go 0 (yrange pos) pos
  where yrange ps = let (minY, maxY) = rangeMap snd ps in maxY - minY
        go t r ps
          | r' > r    = (t, ps)
          | otherwise = go (t + 1) r' ps'
          where r' = yrange ps'
                ps' = forward vel ps
