module Main where

import Day16

main :: IO ()
main = do
           input <- getContents
           let (contexts, instructions) = parseInput input
           let p1 = part1 contexts
           putStrLn $ "Samples behaving like 3+ opcodes: " ++ show p1
           let p2 = part2 contexts instructions
           putStrLn $ "Register 0 after program execution: " ++ show p2
