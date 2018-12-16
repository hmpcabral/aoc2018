module Day16 ( Registers
             , Instruction(..)
             , Context(..)
             , parseInput
             , part1
             , part2
             ) where

import Text.ParserCombinators.ReadP
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Bits ((.&.), (.|.))
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as U
import Data.Set (Set)
import qualified Data.Set as S
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

type Registers = UArray Int Int

data Instruction = Instruction { _op :: Int
                               , _in1 :: Int
                               , _in2 :: Int
                               , _out :: Int }
                 deriving (Show)

data Context = Context { instr :: Instruction
                       , before :: Registers
                       , after :: Registers }
             deriving (Show)

-- Parsing
number :: ReadP Int
number = fmap read $ (skipSpaces >> munch1 isDigit)

instrP :: ReadP Instruction
instrP = do
    (op:in1:in2:out:_) <- count 4 number
    return $ Instruction op in1 in2 out

listOf :: ReadP a -> ReadP [a]
listOf p = between (char '[') (char ']') $ p `sepBy` (char ',' >> skipSpaces)

regP :: ReadP Registers
regP = do
    xs <- listOf number
    guard $ length xs == 4
    return $ U.listArray (0,3) xs

contextP :: ReadP Context
contextP = do
    b <- skipSpaces >> string "Before:" >> skipSpaces >> regP
    i <- instrP
    a <- skipSpaces >> string "After:" >> skipSpaces >> regP
    return $ Context i b a

fileP :: ReadP ([Context], [Instruction])
fileP = do
    ctx <- many1 contextP
    instrs <- manyTill instrP (skipSpaces >> eof)
    return (ctx, instrs)

parseInput :: String -> ([Context], [Instruction])
parseInput = fst . head . readP_to_S fileP

-- Solving
type Opcode = Registers -> Int -> Int -> Int

runOpcode :: Registers -> Instruction -> Opcode -> Registers
runOpcode rs (Instruction _ in1 in2 out) op = rs U.// [(out, op rs in1 in2)]

opcodes :: [(String, Opcode)]
opcodes = [ ("addr", \rs in1 in2 -> rs U.! in1 + rs U.! in2)
          , ("addi", \rs in1 in2 -> rs U.! in1 + in2)
          , ("mulr", \rs in1 in2 -> rs U.! in1 * rs U.! in2)
          , ("muli", \rs in1 in2 -> rs U.! in1 * in2)
          , ("banr", \rs in1 in2 -> rs U.! in1 .&. rs U.! in2)
          , ("bani", \rs in1 in2 -> rs U.! in1 .&. in2)
          , ("borr", \rs in1 in2 -> rs U.! in1 .|. rs U.! in2)
          , ("bori", \rs in1 in2 -> rs U.! in1 .|. in2)
          , ("setr", \rs in1 _ -> rs U.! in1)
          , ("seti", \rs in1 _ -> in1)
          , ("gtir", \rs in1 in2 -> if in1        > rs U.! in2 then 1 else 0)
          , ("gtri", \rs in1 in2 -> if rs U.! in1 > in2        then 1 else 0)
          , ("gtrr", \rs in1 in2 -> if rs U.! in1 > rs U.! in2 then 1 else 0)
          , ("eqir", \rs in1 in2 -> if in1        == rs U.! in2 then 1 else 0)
          , ("eqri", \rs in1 in2 -> if rs U.! in1 == in2        then 1 else 0)
          , ("eqrr", \rs in1 in2 -> if rs U.! in1 == rs U.! in2 then 1 else 0) ]

behavesLike :: Context -> [String]
behavesLike (Context i b a) =
    map fst $ filter ((== a) . runOpcode b i . snd) opcodes

part1 :: [Context] -> Int
part1 = length . filter ((>= 3) . length) . map behavesLike

reverseOpcodes :: [Context] -> IntMap String
reverseOpcodes cs = IM.map (head . S.elems) match'
  where cands = map (\c -> (_op $ instr c, S.fromList $ behavesLike c)) cs
        match = foldr go IM.empty cands
          where go (op, names) = IM.insertWith S.intersection op names

        match' = until (IM.null . IM.filter ((>1) . length)) removeDecided match

        removeDecided m = IM.map (\s -> if S.size s > 1 then s S.\\ d else s) m
          where d = S.unions . IM.elems $ IM.filter ((==1) . length) m

runProgram :: IntMap String -> [Instruction] -> Registers
runProgram ops = foldl' runInstr startReg
  where startReg = U.listArray (0,3) $ repeat 0
        runInstr rs i = runOpcode rs i $ getOp (ops IM.! _op i)
        getOp name = fromJust $ lookup name opcodes

part2 :: [Context] -> [Instruction] -> Int
part2 ctx = (U.! 0) . runProgram (reverseOpcodes ctx)
