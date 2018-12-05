module Day05 where

import Data.Char (toUpper, toLower)
import Data.List (minimumBy)
import Data.Ord (comparing)

type Polymer = String
type Unit = Char

opposite :: Unit -> Unit -> Bool
opposite u v = u /= v && toLower u == toLower v

react :: Polymer -> Polymer
react pol = if length rpol == length pol then rpol else react rpol
  where rpol = foldr react' "" pol
        react' u (x:xs) = if u `opposite` x then xs else (u:x:xs)
        react' u [] = [u]

optimise :: Polymer -> Polymer
optimise pol = minimumBy (comparing length) $ map (react . remove) ['a'..'z']
  where remove u = filter (`notElem` [u, toUpper u]) pol
