module Main where

import Control.Monad (forM_)
import Day10

main :: IO ()
main = do
         input <- getContents
         let (t, msg) = (uncurry message) . readLights $ lines input
         putStrLn $ "Time: " ++ show t
         putStr $ unlines msg
