module Main where

import Data.Char (isSpace)
import Day05

main :: IO ()
main = do
         input <- getContents
         let polymer = filter (not . isSpace) input
         let reacted = react polymer
         putStrLn $ "Length of reacted polymer: " ++ show (length reacted)
         putStrLn $ "Length of optimised polymer: " ++ show (optimise reacted)
