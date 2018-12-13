module Main where

import Linear (V2(..))
import Day13

main :: IO()
main = do
           input <- getContents
           let (tracks, carts) = parseInput $ lines input
           let (V2 y1 x1) = firstCollision tracks carts
           putStrLn $ "First collision at: " ++ show (x1, y1)
           let (V2 y2 x2) = survivor tracks carts
           putStrLn $ "Final survivor at: " ++ show (x2, y2)
