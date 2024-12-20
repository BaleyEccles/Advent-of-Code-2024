module Day1 where

import System.IO  
import Data.List (sort)
import Data.List (group)

day1 = do 
  -- Day 1
  handle <- openFile "./input/Day1.txt" ReadMode
  contents <- hGetContents handle
  let linesOfFile = lines contents
      (leftList, rightList) = unzip $ map (toPair . words) linesOfFile
      
  -- For testing 
  --let leftList = [3, 4, 2, 1, 3, 3]
  --let rightList = [4, 3, 5, 3, 9, 3]
  -- Part 1
                    
  let sortedLeft = sort leftList
  let sortedRight = sort rightList
  let listDifference = [abs(x - y) | (x, y) <- zip sortedLeft sortedRight]

  let sumList = sum listDifference
  print "Solution to part 1:" 
  print sumList
  -- Part 2

  let groupedLeft = group sortedLeft
  let groupedRight = group sortedRight
  
  let groupedRightPair = [(maximum a, a) | a <- groupedRight]
  let groupedLeftPair = [(maximum a, a) | a <- groupedLeft]
  let simScore = sum [(maximum a) * count * (length a) | a <- groupedLeft, let count = numberOfElements a groupedRightPair]
  print "Solution to part 2:" 
  print simScore
  hClose handle

numberOfElements :: [Int] -> [(Int, [Int])] -> Int
numberOfElements a b = length (case lookup (maximum a) b of
                                 Just xs -> xs
                                 Nothing -> [])

toPair :: [String] -> (Int, Int)
toPair [x, y] = (read x, read y)

