module Day02 ( countRepetitions
             , checksum
             , findDistance
             , commonLetters ) where

import qualified Data.Map as M (Map, empty, insertWith, size, filter)

letterCount :: String -> M.Map Char Int
letterCount = foldr (\k m -> M.insertWith (+) k 1 m) M.empty

hasRepetitions :: Int -> String -> Bool
hasRepetitions n str = 0 < M.size (M.filter (== n) count)
  where count = letterCount str

countRepetitions :: Int -> [String] -> Int
countRepetitions rep = length . filter id . map (hasRepetitions rep)

checksum :: [String] -> Int
checksum xs = (countRepetitions 2 xs) * (countRepetitions 3 xs)

hamming :: String -> String -> Int
hamming xs ys = sum $ zipWith (\x y -> if x == y then 0 else 1) xs ys

findDistance :: Int -> [String] -> [String]
findDistance dist xs = [x | x <- xs, y <- xs, hamming x y == dist]

commonLetters :: String -> String -> String
commonLetters xs ys = concat $ zipWith (\x y -> if x == y then [x] else []) xs ys
