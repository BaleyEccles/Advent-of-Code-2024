module Day2 where
import System.IO



day2 = do
  -- Day 2
  handle <- openFile "./input/Day2.txt" ReadMode
  contents <- hGetContents handle
  let linesOfFile = lines contents
      levelsString = (map (words) linesOfFile)
  let levels = toInt levelsString

  -- For testing
  --let levels = [[7, 6, 4, 2, 1],
  --              [1, 2, 7, 8, 9],
  --              [9, 7, 6, 2, 1],
  --              [1, 3, 2, 4, 5],
  --              [8, 6, 4, 4, 1],
  --              [1, 3, 6, 7, 9]]
  
  -- Part 1
  let validLevels = map isValidLevel levels
  let safeReportCount = sum validLevels
  print "Solution to part 1:" 
  print safeReportCount
  
  -- Part 2
  let n = [0 .. (length levels)]
  let levelsWithOneRemoved = map oneRemovedAll levels
  let validLevelsWithRemoved = (map (map isValidLevel) levelsWithOneRemoved)
  let validLevels = map maximum validLevelsWithRemoved
  let safeReportCount = sum validLevels
  print "Solution to part 2:" 
  print safeReportCount

unwrapList :: [Int] -> [Int]
unwrapList xs = xs

oneRemovedAll :: [Int] -> [[Int]]
oneRemovedAll xs = (oneRemoved (xs) 0)
  
oneRemoved :: [Int] -> Int -> [[Int]]
oneRemoved xs n 
  | n == length xs = [xs]
  | otherwise = [(remove n xs)] ++ oneRemoved xs (n + 1)
remove :: Int -> [a] -> [a]
remove n xs = let (as, bs) = splitAt n xs in as ++ tail bs

  
isValidLevel :: [Int] -> Int
isValidLevel [] =  error "empty list"
isValidLevel [x] = 1
isValidLevel (x:xs)
  | abs(x - (xs !! 0)) == 0 = 0
  | abs(x - (xs !! 0)) > 3 = 0
  | x - (xs !! 0) > 0 = isValidLevelIncreasing xs
  | x - (xs !! 0) < 0 = isValidLevelDecreasing xs

isValidLevelIncreasing :: [Int] -> Int
isValidLevelIncreasing [] =  error "empty list"
isValidLevelIncreasing [x] = 1
isValidLevelIncreasing (x:xs)
  | abs(x - (xs !! 0)) == 0 = 0
  | abs(x - (xs !! 0)) > 3 = 0
  | x - (xs !! 0) > 0 = isValidLevelIncreasing xs
  | otherwise = 0

isValidLevelDecreasing :: [Int] -> Int
isValidLevelDecreasing [] =  error "empty list"
isValidLevelDecreasing [x] = 1
isValidLevelDecreasing (x:xs)
  | abs(x - (xs !! 0)) == 0 = 0
  | abs(x - (xs !! 0)) > 3 = 0
  | x - (xs !! 0) < 0 = isValidLevelDecreasing xs
  | otherwise = 0



  
toInt :: [[String]] -> [[Int]]
toInt = map (map read)
