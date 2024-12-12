module Day9 where

import System.IO  
import Data.List (find, nub)
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA
import Control.Parallel.Strategies


type Block = Int

day9 = do
  handle <- openFile "./input/Day9.txt" ReadMode
  file <- hGetContents handle
  let file = "2333133121414131402"
  let diskMap = map convertStringToInt (convertToList (lines file !! 0))
  print diskMap
  let blocks = convertToBlocks diskMap 0
  print blocks
  let compactedFile = compactFile blocks []
  print compactedFile
  let checkSum = getCheckSum compactedFile
  print checkSum
  

getCheckSum :: [Block] -> Int
getCheckSum b = sum (zipWith (*) b [0..((length b)-1)])

compactFile :: [Block] -> [Block] -> [Block]
compactFile b acc
  | length b == 0 = acc
  | b !! 0 /= -1 = compactFile (tail b) (acc ++ [b !! 0])
  | last b == -1 = compactFile (init b) (acc)
  | b !! 0 == -1 = compactFile (tail (init b)) (acc ++ [last b])


--compactFile :: [Block] -> [Block]
--compactFile b
--  | isEmptyBlock splitListSnd == True = ((init splitListFst ++ [last splitListSnd]) ++ (init splitListSnd))
--  | otherwise = compactFile ((init splitListFst ++ [last splitListSnd]) ++ (init splitListSnd))
--  where splitList = splitAt ((findFirstEmptyElementIndex b 0) + 1) b
--        splitListFst = fst splitList
--        splitListSnd = snd splitList

  
isEmptyBlock :: [Block] -> Bool
isEmptyBlock b
  | length (nub b) == 1 = True
  | otherwise = False

  
getLastNum :: [Block] -> Int
getLastNum b
  | last b == -1 = getLastNum (init b)
  | otherwise = last b
  
findFirstEmptyElementIndex :: [Block] -> Int -> Int
findFirstEmptyElementIndex b i
  | b !! i == -1 = i
  | otherwise = findFirstEmptyElementIndex b (i + 1)

convertToBlocks :: [Int] -> Int -> [Block]
convertToBlocks diskMap i
  | i == length diskMap = []
  | isEven i == True =
    (replicate (diskMap !! i) (id)) ++ (convertToBlocks diskMap (i + 1))
  | isEven i == False =
    (replicate (diskMap !! i) (-1)) ++ (convertToBlocks diskMap (i + 1))
  where id = i `div` 2




isEven :: Int -> Bool
isEven n = n `mod` 2 == 0
  
convertStringToInt :: String -> Int
convertStringToInt str = read str

convertToList :: String -> [String]
convertToList str = map (:[]) str
