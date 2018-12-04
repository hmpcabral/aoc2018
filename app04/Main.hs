module Main where

import Day04

main :: IO ()
main = do
         input <- getContents
         let evs = readEvents (lines input)
         let guard = sleepy evs
         putStrLn $ "Sleepiest guard: " ++ show guard
         let (napMin, _) = napTime evs guard
         putStrLn $ "Nap time: " ++ show napMin
         putStrLn $ "Product: " ++ show (guard * napMin)
         let (g, m, c) = sleepiestTime evs
         putStrLn $ "Guard " ++ show g ++ " " ++ show c ++ " times asleep on minute " ++ show m
         putStrLn $ "Product: " ++ show (g * m)
