{-# LANGUAGE ParallelListComp #-}
module Day4 where

import System.IO

day4 = do
  handle <- openFile "./input/Day4.txt" ReadMode
  input <- hGetContents handle
  let wordSearch = lines input
  --print lines
  -- For testing
  --let wordSearch =
  --      ["MMMSXXMASM",
  --       "MSAMXMSMSA",
  --       "AMXSXMAAMM",
  --       "MSAMASMSMX",
  --       "XMASAMXAMM",
  --       "XXAMMXXAMA",
  --       "SMSMSASXSS",
  --       "SAXAMASAAA",
  --       "MAMMMXMMMM",
  --       "MXMXAXMASX"]
  

  -- Part 1
  let list = to2DList wordSearch
  let listX = map addXPos list
  let listY = addYPos listX

  let grid = listY
  let directions = [(0,1),(0,-1),(-1,0),(1,0),(1,1),(1,-1),(-1,-1),(-1,1)]
  let xmasCountBools = [o |
                         y <- [0..length grid - 1],
                         x <- [0..length (fst (grid !! y)) - 1], 
                         d <- directions, 
                         let o = findXmas grid x y d 0]
  let xmasCountTrues = filter removeFalse xmasCountBools
  print "Solution to part 1:" 
  print (length xmasCountTrues)

  -- Part 2
  let crossDirections = [(1, 1), (1, -1), (-1, 1), (-1, -1)]
  let masCount = sum [o |
                       y <- [1..length grid - 2],
                       x <- [1..length (fst (grid !! y)) - 2], 
                       let o = findMasCross grid x y]
  print "Solution to part 2:" 
  print masCount
  
  hClose handle



removeFalse :: Bool -> Bool
removeFalse b
  | b == False = False
  | b == True = True


findXmas :: [([(String, Int)], Int)] -> Int -> Int -> (Int, Int) -> Int -> Bool
findXmas grid x y (dirx, diry) count
  | count == (length xmas) = True
  | x < 0  || y < 0 || y >= length grid || x >= length (fst (grid !! y)) = False
  | (fst (fst (grid !! y) !! x)) == (xmas !! count) = findXmas grid (x + dirx) (y + diry) (dirx, diry) (count + 1)
  | otherwise = False
    where xmas = ["X","M","A","S"]
         

findMasCross :: [([(String, Int)], Int)] -> Int -> Int -> Int
findMasCross grid x y
  | letterAt x y grid == "A" = checkValidCross1 grid x y
  | otherwise = 0

checkValidCross1 :: [([(String, Int)], Int)] -> Int -> Int -> Int
checkValidCross1 grid x y
  | ((letterAt (x + 1) (y + 1) grid == "M" &&  letterAt (x - 1) (y - 1) grid == "S") ||
    (letterAt (x - 1) (y - 1) grid == "M" &&  letterAt (x + 1) (y + 1) grid == "S")) = checkValidCross2 grid x y 
  | otherwise = 0
  
checkValidCross2 :: [([(String, Int)], Int)] -> Int -> Int -> Int
checkValidCross2 grid x y 
  | ((letterAt (x + 1) (y - 1) grid == "M" &&  letterAt (x - 1) (y + 1) grid == "S") ||
    (letterAt (x - 1) (y + 1) grid == "M" &&  letterAt (x + 1) (y - 1) grid == "S")) = 1
  | otherwise = 0
    

    
letterAt :: Int -> Int -> [([(String, Int)], Int)] -> String
letterAt x y grid = (fst (fst (grid !! y) !! x))
  
to2DList :: [String] -> [[String]]
to2DList str = map (map (:[])) str

addXPos :: [String] -> [(String, Int)]
addXPos s = zip s [0..]

addYPos :: [[(String, Int)]] -> [([(String, Int)], Int)]
addYPos s = zip s [0..]
