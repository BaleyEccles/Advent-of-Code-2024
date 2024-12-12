module Day10 where

import System.IO  
import Data.List (find, nub)
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA
import Control.Parallel.Strategies

type Pos = (Int, Int)
type HeightPos = (Int, Pos)

day10 = do
  handle <- openFile "./input/Day10.txt" ReadMode
  file <- hGetContents handle
  --let file = "89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732"
  let heightMap = convertToIntList (lines file)
  print heightMap

  let paths = map (\x -> findPaths x heightMap) heightMap
  print paths
  
  -- For some reason this part is the slow part --
  let score = map getPathScore paths
  print score
  let sumScore = sum score
  print sumScore


getPathScore :: [HeightPos] -> Int
getPathScore h
  | elem 9 onlyHeight == False = 0
  | otherwise = length (nub (filter removeNonMaxHeight h)) 
    where onlyHeight = map (\x -> fst x) h
          removeNonMaxHeight a
            | fst a == 9 = True
            | otherwise = False
  
findPaths :: HeightPos -> [HeightPos] -> [HeightPos]
findPaths heightPos heightMap
  | height /= 0 = []
  | otherwise = findNextStep heightPos heightMap [heightPos]
  where height = fst heightPos
        pos = snd heightPos

findNextStep :: HeightPos -> [HeightPos] -> [HeightPos] -> [HeightPos]
findNextStep heightPos heightMap acc
  | height == 9 = acc
  | otherwise = concatMap (\x -> findNextStep x heightMap (acc ++ [x])) validSurroundingHeights
    where height = fst heightPos
          pos = snd heightPos
          surroundingHeights = getSurroundingHeights heightPos heightMap
          validSurroundingHeights = removeNonValidHeights (map (\x ->
                                           if (fst x) - (height) == 1
                                           then x
                                           else (-10, (-10,-10))) surroundingHeights)

removeNonValidHeights :: [HeightPos] -> [HeightPos]
removeNonValidHeights h = filter removeThem h
  where removeThem h
          | fst h == -10 = False
          | otherwise = True

getSurroundingHeights :: HeightPos -> [HeightPos] -> [HeightPos]
getSurroundingHeights heightPos heightMap = surroundingList
  where surroundingList = [(getHeightPos upPosIndex heightMap), (getHeightPos downPosIndex heightMap), (getHeightPos leftPosIndex heightMap), (getHeightPos rightPosIndex heightMap)]
        posList = convertToOnlyPos heightMap
        upPosIndex = getPosIndex (x, y - 1) posList
        downPosIndex = getPosIndex (x, y + 1) posList
        leftPosIndex = getPosIndex (x - 1, y) posList
        rightPosIndex = getPosIndex (x + 1, y) posList
        x = fst (snd heightPos)
        y = snd (snd heightPos)

getHeightPos :: Maybe Int -> [HeightPos] -> HeightPos
getHeightPos i heightMap
  | i == Nothing = (-10, (-10, -10))
  | otherwise = heightMap !! (fromMaybe 0 i)

convertToOnlyPos :: [HeightPos] -> [Pos]
convertToOnlyPos heightMap = map (\x -> snd x) heightMap

getPosIndex :: Pos -> [Pos] -> Maybe Int
getPosIndex p ps
  | p `elem` ps = Just (getIndex p ps 0)
  | otherwise = Nothing
  where getIndex p ps i
          | ps !! i == p = i
          | otherwise = getIndex p ps (i + 1)
  
convertToIntList :: [String] -> [HeightPos]
convertToIntList s = concatMap (\x -> x) [[(read (int : []), (x, y)) | (int, x) <- zip row [0..]] | (row, y) <- zip s [0..]]
