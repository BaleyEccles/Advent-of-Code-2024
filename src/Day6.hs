module Day6 where

import System.IO  
import Data.List (find)
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA

type Pos = (Int, Int)
type Guard = (String, Pos)
type Maze = [(String, Pos)]

day6 = do
  handle <- openFile "./input/Day6.txt" ReadMode
  program <- hGetContents handle
  let maze = convertToMaze (lines program)
  -- For testing  
  let maze = convertToMaze
             ["....#.....",
              ".........#",
              "..........",
              "..#.......",
              ".......#..",
              "....^.....",
              ".#........",
              "........#.",
              "#.........",
              "......#..."]
  --print maze
  -- Part 1
 
  let numberOfMoves = countMoves maze
  print "Solution to part 1:" 
  print numberOfMoves

  -- Part 2
  -- If turning right gives a place that we have already been and is in the same direction
  -- There is somthing wrong with my method. It doesnt give enough loops, maybe not counting loops that require the use of obstacles that we have not encountered. As this method requires that we touch all obstacles.
  let numberOfLoops = length (countNumberOfLoops maze) 
  print "Solution to part 2:" 

  print numberOfLoops


countNumberOfLoops :: Maze -> [Guard]
countNumberOfLoops m = filter (\x -> isValidObstruction x visitedPosistions) visitedPosistions -- Maybe wrong
  where visitedPosistions = getVisitedPosistions startPos m []
        startPos = backTrackGuard (findGuard m) m


backTrackGuard :: Guard -> Maze -> Guard
backTrackGuard g m
  | x >= maxX || y >= maxY - 1 = g
  | x <= minX || y <= minY - 1= g
  | dir == "^" = backTrackGuard (dir, (x, y + 1)) m
  | dir == ">" = backTrackGuard (dir, (x - 1, y)) m
  | dir == "v" = backTrackGuard (dir, (x, y - 1)) m
  | dir == "<" = backTrackGuard (dir, (x + 1, y)) m
    where (dir, (x, y)) = g
          maxX = getMaxX m
          maxY = getMaxY m
          minX = 0
          minY = 0


isValidObstruction :: Guard -> [Guard] -> Bool
isValidObstruction g vPos
  | dir == "^" && isInList (">", (x + 1,y    )) vPos = True
  | dir == ">" && isInList ("v", (x    ,y + 1)) vPos = True
  | dir == "v" && isInList ("<", (x - 1,y    )) vPos = True
  | dir == "<" && isInList ("^", (x    ,y - 1)) vPos = True
  | otherwise = False
    where (dir, (x, y)) = g

  
  

isInList :: Guard -> [Guard] -> Bool
isInList g vPos = g `elem` vPos

getVisitedPosistions ::  Guard -> Maze -> [Guard] -> [Guard]
getVisitedPosistions g m visitedList
  | fst (snd g) == maxX || snd (snd g) == maxY = visitedList
  | fst (snd g) < minX || snd (snd g) < minY = visitedList
  | fst nextPos == "0" = visitedList
  | fst nextPos == "#" = getVisitedPosistions (rotateGuard g) m visitedList
  | otherwise = getVisitedPosistions (fst g, snd nextPos) m (visitedList ++ [g]) 
  where 
    nextPos = findNextPos g m
    maxX = getMaxX m
    maxY = getMaxY m
    minX = 0
    minY = 0
  
  
countMoves :: Maze -> Int
countMoves m = moveNext startPos m 1 []
    where startPos = findGuard m

moveNext :: Guard -> Maze -> Int -> [Pos] -> Int
moveNext g m i visitedList
  | fst (snd g) == maxX || snd (snd g) == maxY = i
  | fst (snd g) < minX || snd (snd g) < minY = i
  | fst nextPos == "0" = i
  | fst nextPos == "#" = moveNext (rotateGuard g) m i visitedList
  | otherwise =
      if isUniquePos (snd g) visitedList == True
      then moveNext (fst g, snd nextPos) m (i) (visitedList ++ [snd g]) -- Unique position
      else moveNext (fst g, snd nextPos) m (i + 1) (visitedList ++ [snd g]) -- Not unique position
  where 
    nextPos = findNextPos g m
    maxX = getMaxX m
    maxY = getMaxY m
    minX = 0
    minY = 0
        

isUniquePos :: Pos -> [Pos] -> Bool
isUniquePos p ps = p `elem` ps

getMaxX :: Maze -> Int
getMaxX m = maximum (map (\x -> fst (snd x)) m)

getMaxY :: Maze -> Int
getMaxY m = maximum (map (\x -> snd (snd x)) m)
  
  
rotateGuard :: Guard -> Guard
rotateGuard g
  | fst g == "^" = (">", snd g)
  | fst g == "v" = ("<", snd g)
  | fst g == ">" = ("v", snd g)
  | fst g == "<" = ("^", snd g)
    
findNextPos :: Guard -> Maze -> Guard
findNextPos g m =
  let (dir, (x, y)) = g
      nextPos = case dir of
        "^" -> (x, y - 1)  -- Move up
        "v" -> (x, y + 1)  -- Move down
        ">" -> (x + 1, y)  -- Move right
        "<" -> (x - 1, y)  -- Move left
        _   -> (x, y)      -- Default case (no movement)
  in fromMaybe ("0", (0, 0)) (find (\(_, pos) -> pos == nextPos) m)



findGuard :: Maze -> Guard
findGuard m = fromMaybe ("0",(0,0)) (find (\x -> fst x == "^" || fst x == "v" || fst x == ">" || fst x == "<") m)
  
convertToMaze :: [String] -> Maze
convertToMaze s = concatMap (\x -> x) [[(char : [], (x, y)) | (char, x) <- zip row [0..]] | (row, y) <- zip s [0..]]
