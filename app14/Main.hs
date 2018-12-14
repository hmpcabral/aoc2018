module Main where

import Data.Char (digitToInt, isDigit)
import Day14

main :: IO()
main = do
           input <- getContents
           let s1 = part1 (read input)
           putStrLn $ "Part 1: " ++ concat (map show s1)
           let s2 = part2 (map digitToInt $ filter isDigit input)
           putStrLn $ "Part 2: " ++ show s2
