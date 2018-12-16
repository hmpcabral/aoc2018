module Main where

import Data.List (iterate')
import Control.Monad
import Day15

main :: IO ()
main = do
           input <- getContents
           let battle = parseInput $ lines input
           let (rounds, battle') = runBattle battle
           let out = outcome rounds battle'
           putStrLn $ "Outcome after " ++ show rounds ++ " rounds: " ++ show out
           let (up, out') = findPowerup battle
           putStrLn $ "Outcome after powerup " ++ show up ++ ": " ++ show out'
