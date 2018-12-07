module Main where

import Day06

main :: IO ()
main = do
         input <- getContents
         let grid = readGrid (lines input)
         putStrLn $ "Largest finite area: " ++ show (empire grid)
         let boundary = safeBoundary grid 10000
         putStrLn $ "Size of safe area: " ++ show (fmap areaWithin boundary)
