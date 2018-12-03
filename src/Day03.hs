module Day03 ( Claim(..)
             , readClaim
             , countOverlap
             , nonOverlapping ) where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
import Data.Set (Set, empty, fromList, unions, size)

data Claim = Claim { claimId :: Int
                   , claimX :: Int
                   , claimY :: Int
                   , width :: Int
                   , height :: Int }
           deriving (Eq, Show)

claimP :: ReadP Claim
claimP = do
    char '#'
    cid <- number
    skipSpaces
    char '@'
    skipSpaces
    x <- number
    char ','
    y <- number
    char ':'
    skipSpaces
    w <- number
    char 'x'
    h <- number
    eof
    return Claim { claimId = read cid
                 , claimX = read x
                 , claimY = read y
                 , width = read w
                 , height = read h }
  where number = many1 (satisfy isDigit)

readClaim :: String -> Claim
readClaim str = let ((claim, _):_) = readP_to_S claimP str in claim

overlap :: Claim -> Claim -> Set (Int, Int)
overlap c d
  | claimId c == claimId d = empty
  | otherwise              = fromList [(x, y) | x <- ix, y <- iy]
  where ix = intersect (claimX c, width c) (claimX d, width d)
        iy = intersect (claimY c, height c) (claimY d, height d)
        intersect (a1, aw) (b1, bw) = [max a1 b1..min (a1+aw-1) (b1+bw-1)]

overlaps :: [Claim] -> Claim -> Set (Int, Int)
overlaps cs c = unions $ map (overlap c) cs

countOverlap :: [Claim] -> Int
countOverlap cs = size . unions $ map (overlaps cs) cs

nonOverlapping :: [Claim] -> [Claim]
nonOverlapping cs = filter (null . overlaps cs) cs
