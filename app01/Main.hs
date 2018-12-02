module Main where

import Day01

main :: IO ()
main = do
         input <- getContents
         let deltas = toInts input
         putStrLn $ "Final frequency: " ++ show (finalFreq deltas)
         putStrLn $ "First repeated frequency: " ++ show (firstRep deltas)
