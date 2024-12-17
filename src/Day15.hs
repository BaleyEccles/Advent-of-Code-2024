module Day15 where

import System.IO  
import Data.List (nub, intersect, sortBy, group, find, sort)
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA


type Pos = (Int, Int)
type AtPos = (Char, Pos)


day15 = do
  handle1 <- openFile "./input/Day15Part1.txt" ReadMode
  file1 <- hGetContents handle1
  handle2 <- openFile "./input/Day15Part2.txt" ReadMode
  file2 <- hGetContents handle2
  --let file1 = "##########\n#..O..O.O#\n#......O.#\n#.OO..O.O#\n#..O@..O.#\n#O#..O...#\n#O..O..O.#\n#.OO.O.OO#\n#....O...#\n##########"
  --let file2 = "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\nvvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\nv^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

  let directions = (concatMap (\x -> x) (lines file2)) :: [Char]
  let warehouse = convertToData (lines file1)
  print directions
  print warehouse
  let currentPos = getCurrentPos warehouse
  print currentPos
  let finalState = getFinalState warehouse currentPos directions
  print finalState
  let boxes = getBoxes finalState
  print boxes
  let sumGPS = getResult boxes
  print sumGPS

getResult :: [AtPos] -> Int
getResult p = sum (map (\x -> (fst (snd x)) + 100 * (snd (snd x))) p)
  
getBoxes :: [AtPos] -> [AtPos]
getBoxes p = filter (\x -> (fst x) == 'O') p
  

getFinalState :: [AtPos] -> AtPos -> [Char] -> [AtPos]
getFinalState w p d = getNextState w p d

getNextState :: [AtPos] -> AtPos -> [Char] -> [AtPos]
getNextState currentState currentPos (currentDirection:ds)
--  | currentState == nextState = getNextState nextState currentPos ds --nextState
  | ds == [] = currentState
  | otherwise = getNextState nextState currentPos ds
    where nextState = getNewState currentState currentPos currentDirection
          currentPos = getCurrentPos currentState


getNewState :: [AtPos] -> AtPos -> Char -> [AtPos]
getNewState currentState currentPos currentDirection
  | fst currentPos == 'O' && fst objectInFront == '.' = getNewState (movedState) behindObject currentDirection
  | fst objectInFront == '.' = movedState
  | fst objectInFront == 'O' = getNewState currentState objectInFront currentDirection
  | fst objectInFront == '#' = currentState
  | otherwise = []
    where inFrontPos = getInFrontPos currentPos currentDirection
          objectInFront = getObjectFromPos currentState inFrontPos
          movedState = (deleteElement ((deleteElement currentState objectInFront) ++ [(fst currentPos, inFrontPos)]) currentPos) ++ [('.', snd currentPos)]
          behindPos = getBehindPos currentPos currentDirection
          behindObject = getObjectFromPos currentState behindPos


getBehindPos :: AtPos -> Char -> Pos
getBehindPos currentPos direction
  | direction == '^' = (fst pos, snd pos + 1)
  | direction == 'v' = (fst pos, snd pos - 1)
  | direction == '>' = (fst pos - 1, snd pos)
  | direction == '<' = (fst pos + 1, snd pos)
  where pos = snd currentPos


deleteElement :: [AtPos] -> AtPos -> [AtPos]
deleteElement state elem = filter (\x -> x /= elem) state

getInFrontPos :: AtPos -> Char -> Pos
getInFrontPos currentPos direction
  | direction == '^' = (fst pos, snd pos - 1)
  | direction == 'v' = (fst pos, snd pos + 1)
  | direction == '>' = (fst pos + 1, snd pos)
  | direction == '<' = (fst pos - 1, snd pos)
  where pos = snd currentPos

getObjectFromPos :: [AtPos] -> Pos -> AtPos
getObjectFromPos currentState p = fromMaybe defaultPos (find (\x -> snd x == p) currentState)
  where
    defaultPos = ('N', (0,0))


getCurrentPos :: [AtPos] -> AtPos
getCurrentPos w = fromMaybe nullPos (find (\x -> fst x == '@') w)
  where
    nullPos = ('N', (0,0))
  
convertToData :: [String] -> [AtPos]
convertToData s = map toChar (concatMap (\x -> x) [[(char : [], (x, y)) | (char, x) <- zip row [0..]] | (row, y) <- zip s [0..]])


toChar :: (String, Pos) -> AtPos
toChar a = ((fst a) !! 0, snd a)
