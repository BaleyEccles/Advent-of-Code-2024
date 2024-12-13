module Day11 where

import System.IO  
import Data.List
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA
import Control.Parallel.Strategies

type Pos = (Int, Int)
type HeightPos = (Int, Pos)

day11= do
  let input = [9759, 0, 256219, 60, 1175776, 113, 6, 92833]
  -- For testing 
  --let input = [125, 17]
  print input


  -- Part 1
  let output = findNextBlink input 25
  --print output
  let len = length output
  print len
  
  ---- Part 2
  --let output = 
  ----print output
  --let len = length output
  --print len
findNextBlink :: [Int] -> Int -> [Int]
findNextBlink b depth 
  | depth == 0 = b
  | otherwise = findNextBlink (concatMap applyRule b) (depth - 1)

applyRule :: Int -> [Int]
applyRule b
  | b == 0 = [1]
  | (lengthOfInt b `mod` 2) == 0 = getHalvesOfInt b
  | otherwise = [b * 2024]

  
getHalvesOfInt :: Int -> [Int]
getHalvesOfInt b = [(read (fst split)), (read (snd split))]
  where s = (lengthOfInt b) `div` 2
        split = (splitAt s (show b))
  



lengthOfInt :: Int -> Int
lengthOfInt 0 = 1  -- Special case for 0
lengthOfInt n = length (show n)
