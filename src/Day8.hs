module Day8 where

import System.IO  
import Data.List (find)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA

type Pos = (Int, Int)
type Signal = (String, Pos)

day8 = do
  handle <- openFile "./input/Day8.txt" ReadMode
  file <- hGetContents handle
  -- For testing
  -- let file = "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............"
  --let file = ".\n.\na\na\n.\n."
  --let file = "....a..a...."
  let signals = convertToSignalList (lines file)
  print signals
  -- Part 1
  let antiNodes = updateSignalWithAntiNodes signals
  --print antiNodes
  let onlyNodes = filter convertToMap antiNodes
  print onlyNodes
  let onlyAnitNodes = filter removeNotAntiNodes antiNodes
  print onlyAnitNodes
  let onlyValidAntiNodes = filter (\a -> removeOutOfBounds signals a) onlyAnitNodes
  print onlyValidAntiNodes
  let noDuplicates = nub onlyValidAntiNodes
  let l = length noDuplicates
  print l 
        

removeOutOfBounds :: [Signal] -> Signal -> Bool
removeOutOfBounds s s1
  | x < 0 = False
  | y < 0 = False
  | x > maxX = False
  | y > maxY = False
  | otherwise = True
  where x = fst (snd s1)
        y = snd (snd s1)
        maxX = findMaxX s
        maxY = findMaxY s


convertToMap :: Signal -> Bool
convertToMap s
  | fst s == "." = False
  | otherwise = True
  
removeNotAntiNodes :: Signal -> Bool
removeNotAntiNodes s
  | fst s == "#" = True
  | otherwise = False

updateSignalWithAntiNodes :: [Signal] -> [Signal]
updateSignalWithAntiNodes s = s ++ (concatMap (\a -> a) [o |
                                                         y <- [0..maxY],
                                                         x <- [0..maxX], 
                                                         let o = checkIfFrequency s x y])
  where maxY = findMaxY s
        maxX = findMaxX s

checkIfFrequency :: [Signal] -> Int -> Int -> [Signal]
checkIfFrequency s x y -- = [("#", (maxX, maxY))]
  | isFrequency currentSignal == True = findAntiNode s x y
  | otherwise = [currentSignal]
  where currentSignal = (s !! currentIdx)
        currentIdx = x + (y * (maxX + 1))
        maxX = findMaxX s
        maxY = findMaxY s

isFrequency :: Signal -> Bool
isFrequency s
  | fst s == "." = False
  | otherwise = True
          
findAntiNode :: [Signal] -> Int -> Int -> [Signal]  
findAntiNode s x1 y1 = -- [("#", (maxX, maxY))]
--  concatMap (\a -> a) [o |
--                       y2 <- [0..maxY],
--                       x2 <- [0..maxX], 
--                       let o = getAntiNodeFromFrequecny s currentSignal x2 y2]
  [o |
    y2 <- [0..maxY],
    x2 <- [0..maxX], 
    let o = getAntiNodeFromFrequecny s currentSignal x2 y2]
  where maxY = findMaxY s
        maxX = findMaxX s
        currentSignal = (s !! currentIdx)
        currentIdx = x1 + (y1 * (maxX + 1))


getAntiNodeFromFrequecny :: [Signal] -> Signal -> Int -> Int -> Signal
getAntiNodeFromFrequecny s s1 x2 y2
--   | snd s1 /= snd secondSignal && (fst s1) == (fst secondSignal) = map (\a -> createAntiNode ((x1 + a*2*(x2 - x1))) ((y1 + a*2*(y2 - y1)))) part2List
  | snd s1 /= snd secondSignal && (fst s1) == (fst secondSignal) = createAntiNode (x1 + 2*(x2 - x1)) (y1 + 2*(y2 - y1))
  | otherwise = [(".", (maxX, maxY))] !! 0
  where secondSignal = (s !! currentIdx)
        currentIdx = x2 + (y2 * (maxX + 1))
        maxX = findMaxX s
        maxY = findMaxY s
        x1 = fst (snd s1)
        y1 = snd (snd s1)
        part2List = [0..100]
          
createAntiNode :: Int -> Int -> Signal
createAntiNode x y = ("#", (x, y))
  
findMaxX :: [Signal] -> Int
findMaxX s = maximum (map (\a -> fst (snd a)) s)
findMaxY :: [Signal] -> Int
findMaxY s = maximum (map (\a -> snd (snd a)) s)


convertToSignalList :: [String] -> [Signal]
convertToSignalList s = concatMap (\x -> x) [[(char : [], (x, y)) | (char, x) <- zip row [0..]] | (row, y) <- zip s [0..]]
