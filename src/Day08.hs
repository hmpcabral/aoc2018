module Day08 (Tree(..), readTree, addMetadata, value) where

import Control.Arrow (first)

data Tree = Tree { getChildren :: [Tree]
                 , getMeta :: [Int] }
          deriving (Show)

-- Parsing
buildTree :: [Int] -> (Tree, [Int])
buildTree (nc:nm:rest) = (Tree (reverse children) meta, rest'')
  where (meta, rest'') = splitAt nm rest'
        (children, rest') = (!! nc) $ iterate addChild ([], rest)
        addChild (cs, xs) = first (:cs) $ buildTree xs

readTree :: String -> Tree
readTree = fst . buildTree . map read . words

-- Part 1
addMetadata :: Tree -> Int
addMetadata (Tree cs ms) = sum ms + sum (map addMetadata cs)

-- Part 2
value :: Tree -> Int
value (Tree [] ms) = sum ms
value t@(Tree _ ms) = sum $ map (metaValue t) ms
  where metaValue (Tree cs _) idx
          | idx == 0        = 0
          | idx > length cs = 0
          | otherwise       = value (cs !! (idx - 1))
