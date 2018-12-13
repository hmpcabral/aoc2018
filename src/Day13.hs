module Day13 where

import Data.Maybe (isJust, fromJust)
import Data.List (foldl', tails)
import Data.Map (Map)
import qualified Data.Map as M
import Linear (V2(..))

data Direction = Up | Down | DLeft | DRight
               deriving (Eq, Show)

data Turn = Straight | TLeft | TRight
          deriving (Eq, Show)

data Cart = Cart { heading :: Direction
                 , nextTurn :: Turn }
          | Collision
          deriving (Eq, Show)

type Tracks = Map (V2 Int) (Cart -> Cart)
type Positions = Map (V2 Int) Cart

-- Movement
move :: Direction -> V2 Int -> V2 Int
move Up     = (+ V2 (-1)  0)
move Down   = (+ V2   1   0)
move DLeft  = (+ V2   0 (-1))
move DRight = (+ V2   0   1)

turn :: Turn -> Direction -> Direction
turn Straight d      = d
turn TRight   DLeft  = Up
turn TLeft    DRight = Up
turn TLeft    DLeft  = Down
turn TRight   DRight = Down
turn TLeft    Up     = DLeft
turn TRight   Down   = DLeft
turn TRight   Up     = DRight
turn TLeft    Down   = DRight

fwdTurn :: Turn -> Turn
fwdTurn Straight = TRight
fwdTurn TRight   = TLeft
fwdTurn TLeft    = Straight

-- Pieces of track
cornerNWtoSE :: Cart -> Cart
cornerNWtoSE (Cart Up     nt) = Cart DRight nt
cornerNWtoSE (Cart DLeft  nt) = Cart Down nt
cornerNWtoSE (Cart Down   nt) = Cart DLeft nt
cornerNWtoSE (Cart DRight nt) = Cart Up nt

cornerNEtoSW :: Cart -> Cart
cornerNEtoSW (Cart Up     nt) = Cart DLeft nt
cornerNEtoSW (Cart DLeft  nt) = Cart Up nt
cornerNEtoSW (Cart Down   nt) = Cart DRight nt
cornerNEtoSW (Cart DRight nt) = Cart Down nt

intersection :: Cart -> Cart
intersection (Cart h nt) = Cart (nt `turn` h) (fwdTurn nt)

-- Parsing
parseLine :: Int -> String -> (Tracks, Positions)
parseLine y l = (tracks, carts)
  where (_, tracks, carts) = foldl' acc (V2 y 0, M.empty, M.empty) $ tails l
        acc (p, ts, ps) xs = (p + V2 0 1, ts', ps')
          where (ts', ps') = parse p ts ps xs

        parse p ts ps (x:_)
          | isJust d = (ts, M.insert p (Cart (fromJust d) TLeft) ps)
          | isJust t = (M.insert p (fromJust t) ts, ps)
          where d = direction x
                t = track x
        parse _ ts ps _ = (ts, ps)

        track '/'  = Just cornerNWtoSE
        track '\\' = Just cornerNEtoSW
        track '+'  = Just intersection
        track _    = Nothing

        direction '>' = Just DRight
        direction '<' = Just DLeft
        direction 'v' = Just Down
        direction '^' = Just Up
        direction _   = Nothing

parseInput :: [String] -> (Tracks, Positions)
parseInput ls = (M.unions ts, M.unions ps)
  where (ts, ps) = unzip . map (uncurry parseLine) $ zip [0..] ls

-- Solving
tick :: Tracks -> Positions -> Positions
tick tracks carts = M.foldlWithKey' acc M.empty carts
  where acc carts' p c = M.insertWith (\_ _ -> Collision) p' c' carts'
          where p' | p `M.member` carts' = p
                   | otherwise           = move (heading c) p
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
