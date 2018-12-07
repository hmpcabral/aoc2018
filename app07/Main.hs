module Main where

import Day07
import Data.Char (ord)

main :: IO ()
main = do
         input <- getContents
         let instructions = readManual (lines input)
         putStrLn $ "Completion order: " ++ order instructions
         let time = parallelWork 5 (\c -> ord c - ord 'A' + 61) instructions
         putStrLn $ "5 workers finish in: " ++ show time
