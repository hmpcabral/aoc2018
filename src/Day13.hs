module Day13 where

import Data.Maybe (isJust, fromJust)
import Data.List (foldl', tails)
import Data.Map (Map)
import qualified Data.Map as M
import Linear (V2(..))
import Linear.Matrix (identity, transpose, (!*))

data Cart = Cart { heading :: V2 Int
                 , nextTurn :: V2 (V2 Int) }
          | Collision
          deriving (Eq, Show)

type Tracks = Map (V2 Int) (Cart -> Cart)
type Positions = Map (V2 Int) Cart

-- Movement
up    = V2 (-1)  0
down  = V2   1   0
left  = V2   0 (-1)
right = V2   0   1

straight :: V2 (V2 Int)
straight = identity

turnLeft = V2 (V2 0 (-1)) (V2 1 0)
turnRight = transpose turnLeft

fwdTurn :: V2 (V2 Int) -> V2 (V2 Int)
fwdTurn t
  | t == straight  = turnRight
  | t == turnRight = turnLeft
  | t == turnLeft  = straight

-- Pieces of track
cornerNWtoSE :: Cart -> Cart
cornerNWtoSE (Cart d nt)
  | d == up    = Cart right nt
  | d == left  = Cart down nt
  | d == down  = Cart left nt
  | d == right = Cart up nt

cornerNEtoSW :: Cart -> Cart
cornerNEtoSW (Cart d nt)
  | d == up    = Cart left nt
  | d == left  = Cart up nt
  | d == down  = Cart right nt
  | d == right = Cart down nt

intersection :: Cart -> Cart
intersection (Cart h nt) = Cart (nt !* h) (fwdTurn nt)

-- Parsing
parseLine :: Int -> String -> (Tracks, Positions)
parseLine y l = (tracks, carts)
  where (_, tracks, carts) = foldl' acc (V2 y 0, M.empty, M.empty) $ tails l
        acc (p, ts, ps) xs = (p + V2 0 1, ts', ps')
          where (ts', ps') = parse p ts ps xs

        parse p ts ps (x:_)
          | isJust d = (ts, M.insert p (Cart (fromJust d) turnLeft) ps)
          | isJust t = (M.insert p (fromJust t) ts, ps)
          where d = direction x
                t = track x
        parse _ ts ps _ = (ts, ps)

        track '/'  = Just cornerNWtoSE
        track '\\' = Just cornerNEtoSW
        track '+'  = Just intersection
        track _    = Nothing

        direction '>' = Just right
        direction '<' = Just left
        direction 'v' = Just down
        direction '^' = Just up
        direction _   = Nothing

parseInput :: [String] -> (Tracks, Positions)
parseInput ls = (M.unions ts, M.unions ps)
  where (ts, ps) = unzip . map (uncurry parseLine) $ zip [0..] ls

-- Solving
tick :: Tracks -> Positions -> Positions
tick tracks carts = M.foldlWithKey' acc M.empty carts
  where acc carts' p c = M.insertWith (\_ _ -> Collision) p' c' carts'
          where p' | p `M.member` carts' = p
                   | otherwise           = p + heading c
                c' | p' `M.member` carts = Collision
                   | otherwise           = M.findWithDefault id p' tracks $ c

headPos :: Positions -> V2 Int
headPos = fst . head . M.assocs

-- Part 1
firstCollision :: Tracks -> Positions -> V2 Int
firstCollision tracks carts = headPos collided
  where carts' = until ((Collision `elem`) . M.elems) (tick tracks) carts
        collided = M.filter (== Collision) carts'

-- Part 2
survivor :: Tracks -> Positions -> V2 Int
survivor tracks carts = headPos $ until ((<= 1) . M.size) tickClean carts
  where tickClean = M.filter (/= Collision) . tick tracks
