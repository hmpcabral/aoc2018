module Main where

import Day12

main :: IO ()
main = do
         input <- getContents
         let (rules, garden) = parseInput input
         let a1 = aliveAt rules garden 20
         putStrLn $ "Alive after 20 generations: " ++ show a1
         let a2 = aliveAt rules garden 50000000000
         putStrLn $ "Alive after 5e10 generations: " ++ show a2
