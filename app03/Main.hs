module Main where

import Day03

main :: IO ()
main = do
  input <- getContents
  let claims = map readClaim (lines input)
  putStrLn $ "Overlapping square inches: " ++ show (countOverlap claims)
  let (intact:_) = nonOverlapping claims
  putStrLn $ "Nonoverlapping claim: " ++ show (claimId intact)
