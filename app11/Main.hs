module Main where

import Day11

main :: IO ()
main = do
         input <- getContents
         let serial = read input
         let (_, c1) = maxSquareOf serial 3
         putStrLn $ "3x3 square with most power at: " ++ show c1
         let ((x2, y2), sz) = maxSquare serial
         putStrLn $ "Maximum power: " ++ show (x2, y2, sz)
