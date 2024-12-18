module Day17 where

import System.IO  
import Data.List (nub, intersect, sortBy, group, find, sort)
import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.TDFA
import Data.Bits

type Register = Int
type Registers = (Int, Int, Int)
type ProgramCounter = Int
type Program = [Int]
type ProgramState = (Registers, Program, ProgramCounter)

day17 = do
  -- Part 1
  let a = 44348299
  let b = 0
  let c = 0
  let program = [2,4,1,5,7,5,1,6,0,3,4,2,5,5,3,0]
  let output = runProgram ((a, b, c), program, 0) []
  print output
  -- Part 2 -- 4:58:45 PM
  let a = 0
  let b = 0
  let c = 0
  let program = [2,4,1,5,7,5,1,6,0,3,4,2,5,5,3,0]
  let output = getA ((a, b, c), program, 0) [0..]
  print output

getA :: ProgramState -> [Int] -> Int
getA ((_, b, c), program, pc) (a:as)
  | runProgram ((a, b, c), program, 0) [] == program = a
  | otherwise = getA ((0, b, c), program, 0) as



runProgram :: ProgramState -> [Int] -> [Int]
runProgram ((a, b, c), p, pc) output
  | pc >= length p = output
  | p !! pc == 0 = runProgram (adv ((a, b, c), p, pc)) output
  | p !! pc == 1 = runProgram (bxl ((a, b, c), p, pc)) output
  | p !! pc == 2 = runProgram (bst ((a, b, c), p, pc)) output
  | p !! pc == 3 = runProgram (jnz ((a, b, c), p, pc)) output
  | p !! pc == 4 = runProgram (bxc ((a, b, c), p, pc)) output
  | p !! pc == 5 = runProgram ((a, b, c), p, (pc + 2)) (output ++ [out ((a, b, c), p, pc)])
  | p !! pc == 6 = runProgram (bdv ((a, b, c), p, pc)) output
  | p !! pc == 7 = runProgram (cdv ((a, b, c), p, pc)) output

adv :: ProgramState -> ProgramState
adv ((a, b, c), p, pc) = ((newA, b, c), p, (pc + 2))
  where numerator = a
        denominator = 2 ^ (getComboOperand ((a, b, c), p, pc) (p !! (pc + 1)))
        newA = (numerator `div` denominator) -- May not be `truncated`

bxl :: ProgramState -> ProgramState
bxl ((a, b, c), p, pc) = ((a, newB, c), p, (pc + 2))
  where newB = xor b (p !! (pc + 1)) -- May need to deal with xor stuff

bst :: ProgramState -> ProgramState
bst ((a, b, c), p, pc) = ((a, newB, c), p, (pc + 2))
  where newB = (getComboOperand ((a, b, c), p, pc) (p !! (pc + 1))) `mod` 8

jnz :: ProgramState -> ProgramState
jnz ((a, b, c), p, pc)
  | a == 0 = ((a, b, c), p, pc + 2)
  | pc == p !! (pc + 1) = ((a, b, c), p, (pc + 2))
  | otherwise = ((a, b, c), p, p !! (pc + 1))

bxc :: ProgramState -> ProgramState
bxc ((a, b, c), p, pc) = ((a, newB, c), p, (pc + 2))
  where newB = xor b c

bdv :: ProgramState -> ProgramState
bdv ((a, b, c), p, pc) = ((a, newB, c), p, (pc + 2))
  where numerator = a
        denominator = 2 ^ (getComboOperand ((a, b, c), p, pc) (p !! (pc + 1)))
        newB = (numerator `div` denominator) -- May not be `truncated`

cdv :: ProgramState -> ProgramState
cdv ((a, b, c), p, pc) = ((a, b, newC), p, (pc + 2))
  where numerator = a
        denominator = 2 ^ (getComboOperand ((a, b, c), p, pc) (p !! (pc + 1)))
        newC = (numerator `div` denominator) -- May not be `truncated`        

out :: ProgramState -> Int
out ((a, b, c), p, pc) = output
  where output = (getComboOperand ((a, b, c), p, pc) (p !! (pc + 1))) `mod` 8


getComboOperand :: ProgramState -> Int -> Int
getComboOperand ((a, b, c), p, pc) input
  | input == 4 = a
  | input == 5 = b
  | input == 6 = c
  | otherwise = input



