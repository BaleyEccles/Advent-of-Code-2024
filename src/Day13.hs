module Day13 where

import System.IO  
import Data.List (nub, intersect, sortBy, group, find)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA
import Control.Parallel.Strategies (parList, rseq, parMap)
import Numeric.LinearAlgebra

type Pos = (Int, Int)
type Machine = [Pos]

day13 = do
  handle <- openFile "./input/Day13.txt" ReadMode
  file <- hGetContents handle
  --For testing
  --let file = "Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400\n\nButton A: X+26, Y+66\nButton B: X+67, Y+21\nPrize: X=12748, Y=12176\n\nButton A: X+17, Y+86\nButton B: X+84, Y+37\nPrize: X=7870, Y=6450\n\nButton A: X+69, Y+23\nButton B: X+27, Y+71\nPrize: X=18641, Y=10279"
  let segments = splitOn "\n\n" file
  --print segments
  let machines = map convertToPair segments
  ---print machines
  let solutionsDouble = map findSolutions machines
  --print solutionsDouble
  let solutions = map (map convertToInt . toList) (map findSolutions machines)
  --print solutions
  let totalTokens = sum (map getTokens solutions)
  print totalTokens
  -- Part 2
  --Need to solve the system of equations
  -- Nx*Ax + Ny*Bx = LARGENUMBER + X
  -- Nx*Ay + Ny*By = LARGENUMBER + Y
  -- By simply adding the large number and solving wont work.
  let machinesP2 = map addLargeNum (map convertToPair segments)
  --print machinesP2
  let solutionsDoubleP2 = map findSolutions machinesP2
  --print solutionsDoubleP2
  let solutionsP2 = map (map convertToInt . toList) (map findSolutions machinesP2)
  --print solutionsP2
  let totalTokensP2 = sum (map getTokens solutionsP2)
  --print totalTokensP2

addLargeNum :: Machine -> Machine
addLargeNum m = [a, b, c]
  where a = m !! 0
        b = m !! 0
        c = ((fst (m !! 0) + 10000000000000), (snd (m !! 0) + 10000000000000))


getTokens :: [Maybe Integer] -> Integer
getTokens moves
  | a == Nothing = 0
  | a == Nothing = 0
  | otherwise = 3*(fromMaybe 0 a) + (fromMaybe 0 b) 
  where a = moves !! 0
        b = moves !! 1
  
convertToInt :: Double -> Maybe Integer
convertToInt a
  | abs ((fromIntegral (round a) :: Double) - a) < tol = Just (round a)
  | otherwise = Nothing
  where tol = 0.00001


             
--Need to solve the system of equations
-- Nx*Ax + Ny*Bx = X
-- Nx*Ay + Ny*By = Y
solveSystem :: Matrix Double -> Vector Double -> Vector Double
solveSystem a b = a <\> b
  
findSolutions :: Machine -> Vector Double
findSolutions m = solveSystem a b
  where
    (ax, ay) = m !! 0
    (bx, by) = m !! 1
    (sa, sb) = m !! 2
    eq1 = [fromIntegral ax, fromIntegral bx]
    eq2 = [fromIntegral ay, fromIntegral by]
    sol = [fromIntegral sa, fromIntegral sb]
    a = (2><2) (eq1 ++ eq2)
    b = fromList sol

convertToPair :: String -> Machine
convertToPair line = [(read (numbers !! 0), read (numbers !! 1)), 
                      (read (numbers !! 2), read (numbers !! 3)), 
                      (read (numbers !! 4), read (numbers !! 5))]
  where numbers = doRegex line "[[:digit:]]+"
  
doRegex :: String -> String -> [String]
doRegex str regex = getAllTextMatches (str =~ regex) :: [String]
