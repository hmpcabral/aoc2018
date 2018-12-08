module Main where

import Day08

main :: IO ()
main = do
         input <- getContents
         let tree = readTree input
         putStrLn $ "Sum of metadata: " ++ show (addMetadata tree)
         putStrLn $ "Value of root node: " ++ show (value tree)
