module Main where

import Day02

main :: IO ()
main = do
         input <- getContents
         let ids = lines input
         putStrLn $ "Checksum: " ++ show (checksum ids)

         let (id1:id2:_) = findDistance 1 ids
         putStrLn $ "Common letters: " ++ commonLetters id1 id2
