module Main where

import Day04

main :: IO ()
main = do
         input <- getContents
         let evs = readEvents (lines input)
         let (guard, minute) = part1 evs
         putStrLn $ "Part 1: " ++ show (guard * minute)
         let (guard', minute') = part2 evs
         putStrLn $ "Part 2: " ++ show (guard' * minute')
