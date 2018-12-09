module Main where

import Data.Char (isDigit)
import Day09

main :: IO ()
main = do
         input <- getContents
         let [players, marbles] = map read . filter (all isDigit) $ words input
         let score = highScore players marbles
         putStrLn $ "High score: " ++ show score
         let bigScore = highScore players (marbles * 100)
         putStrLn $ "High score (100x): " ++ show bigScore
