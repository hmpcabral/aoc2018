module Day07 (Manual, readManual, order, parallelWork) where

import Text.ParserCombinators.ReadP
import Data.List (sort)
import qualified Data.Map as M

type Manual = M.Map Char [Char]

-- Parsing
dependencyP :: ReadP (Char, Char)
dependencyP = do
    string "Step "
    pre <- get
    string " must be finished before step "
    post <- get
    string " can begin."
    eof
    return (pre, post)

readManual :: [String] -> Manual
readManual = foldr (\(b, a) -> append a [] . append b [a]) M.empty . map parse
  where append = M.insertWith (++)
        parse str = let ((dep, _):_) = readP_to_S dependencyP str in dep

-- Part 1
order :: Manual -> [Char]
order todo
  | todo == M.empty = []
  | otherwise       = next:order (M.delete next todo)
  where next = minimum [k | k <- M.keys todo, all (k `notElem`) (M.elems todo)]

-- Part 2
data State = State { getClock :: Int
                   , getTodo :: M.Map Char [Char]
                   , getOngoing :: M.Map Char Int }
           deriving (Show)

forward :: Int -> (Char -> Int) -> State -> State
forward workers df (State clock todo ongoing) = State clock' todo' ongoing''
  where clock' = let ends = M.elems ongoing''
                 in if null ends then clock + 1 else minimum ends

        ongoing'' = foldr (\t -> M.insert t (clock + df t)) ongoing' next

        next = take (workers - M.size ongoing') $ sort free
        free = filter (\t -> all (t `notElem`) (M.elems todo')) $ M.keys idle
        idle = todo' M.\\ ongoing'

        todo' = todo M.\\ done
        (ongoing', done) = M.partition (> clock) ongoing

parallelWork :: Int -> (Char -> Int) -> Manual -> Int
parallelWork workers df todo = getClock $ until done (forward workers df) start
  where done = (== M.empty) . getTodo
        start = State (-1) todo M.empty
