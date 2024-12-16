module Day14 where

import System.IO  
import Data.List (nub, intersect, sortBy, group, find, sort)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA
import Control.Parallel.Strategies (parList, rseq, parMap)
import Numeric.LinearAlgebra

type Pos = (Int, Int)
type Vel = (Int, Int)


day14 = do
  handle <- openFile "./input/Day14.txt" ReadMode
  file <- hGetContents handle
  let width = 101
  let height = 103
  let time = 100
  --For testing
  --let file = "p=0,4 v=3,-3\np=6,3 v=-1,-3\np=10,3 v=-1,2\np=2,0 v=2,-1\np=0,0 v=1,3\np=3,0 v=-2,-2\np=7,6 v=-1,-3\np=3,0 v=-1,-2\np=9,3 v=2,3\np=7,3 v=-1,2\np=2,4 v=2,-3\np=9,5 v=-3,-3"
  --let width = 11
  --let height = 7
  --let time = 100

  let posVel = map convertToPosVel (map (\x -> doRegex x "[[:digit:]]+|-[[:digit:]]+") (lines file))
  print posVel
  let finPos = map (\x -> findlastPos x width height time) posVel
  print finPos
  let quadCount = countElements (map (\x -> countQuad x width height) finPos)
  print quadCount
  let safetyFactor = getSafetyFactor quadCount
  print safetyFactor
  -- Part 2
  
  

getSafetyFactor :: [(Int, String)] -> Int
getSafetyFactor q = foldl (*) 1 (map (\x -> getCount x) q)
  where getCount q
          | snd q == "None" = 1
          | otherwise = fst q

                             
  
countElements ::  [String] -> [(Int, String)]
countElements xs = map (\ys -> (length ys, head ys)) grouped
  where
    grouped = group (sort xs)

countQuad :: Pos -> Int -> Int -> String
countQuad pos w h
  | x < xMid && y < yMid = "Top Left"
  | x < xMid && y > yMid = "Bottom Left"
  | x > xMid && y < yMid = "Top Right"
  | x > xMid && y > yMid = "Bottom Right"
  | otherwise = "None"
  where xMid = (w - 1) `div` 2
        yMid = (h - 1) `div` 2
        x = fst pos
        y = snd pos

findlastPos :: (Pos, Vel) -> Int -> Int -> Int -> Pos
findlastPos pv w h t = (finalx, finaly)
  where pos = fst pv
        x = fst pos
        y = snd pos
        vel = snd pv
        vx = fst vel
        vy = snd vel
        finalx = (x + vx*t) `mod` w
        finaly = (y + vy*t) `mod` h
  
  
  
convertToPosVel :: [String] -> (Pos, Vel)
convertToPosVel s = ((x, y),(vx, vy))
  where intVelPos = map read s
        x = intVelPos !! 0
        y = intVelPos !! 1
        vx = intVelPos !! 2
        vy = intVelPos !! 3

    
doRegex :: String -> String -> [String]
doRegex str regex = getAllTextMatches (str =~ regex) :: [String]
  
