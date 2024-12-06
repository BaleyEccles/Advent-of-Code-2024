module Day5 where

import System.IO
import Text.Regex.TDFA
import Data.Maybe (catMaybes, fromMaybe)
import Control.Parallel.Strategies (parList, rseq, using)

type PageOrderingRule = (Int, Int)
type Update = [Int]

day5 = do
  -- For testing
  handle1 <- openFile "./input/Day5Part1Test.txt" ReadMode
  --handle1 <- openFile "./input/Day5Part1.txt" ReadMode
  input1 <- hGetContents handle1
  let segment1 = lines input1
  let pageOrderingRules = map convertToPageOrderingRule segment1
  --print pageOrderingRules

  -- For testing
  handle2 <- openFile "./input/Day5Part2Test.txt" ReadMode
  --handle2 <- openFile "./input/Day5Part2.txt" ReadMode
  input2 <- hGetContents handle2
  let segment2 = lines input2
  let updates = map convertToUpdate segment2

  -- Part 1
  let pageIsValid = map (\update -> verifyUpdate update pageOrderingRules 0) updates

  let middleElement = zipWith getMiddleElement pageIsValid updates
  let sumOfMiddleElements = sum middleElement
  
  print sumOfMiddleElements
  -- Part 2
  let invalidElements = catMaybes $ zipWith removeValidElements pageIsValid updates
  --print invalidElements
  -- map (\update -> fixUpdate update pageOrderingRules 0) invalidElements
  let fixedPages = map (\x -> fixUpdate x pageOrderingRules 0) invalidElements
  --let fixedPages = map (\x -> fixUpdate x pageOrderingRules 0) invalidElements `using` parList rseq
  print fixedPages


  let middleOfFixedPages = map getMiddleOfList fixedPages
  let sumOfMiddleOfFixedPages = sum middleOfFixedPages
  print sumOfMiddleOfFixedPages
  

removeValidElements :: Bool -> Update -> Maybe Update
removeValidElements b u
  | b == False = Just u
  | otherwise = Nothing

getMiddleOfList :: [Int] -> Int
getMiddleOfList a = a !! (((length a) - 1) `div` 2)
  
getMiddleElement :: Bool -> Update -> Int
getMiddleElement b u
  | b == True = u !! (((length u) - 1) `div` 2)
  | otherwise = 0



fixUpdate :: Update -> [PageOrderingRule] -> Int -> Update
fixUpdate u r i
  | elem updatePairs r == False = swapAndVerifyUpdate u r i 1
  | otherwise = fixUpdate u r (i + 1)
  where updatePairs = (u !! i, u !! (i + 1))


swapAndVerifyUpdate :: Update -> [PageOrderingRule] -> Int -> Int -> Update
swapAndVerifyUpdate u r i j
  | verifyUpdate fixedUpdate r 0 == True = fixedUpdate
  | j >= (length u) - 1 = swapAndVerifyUpdateWithMoreThanOneError u r i
  -- fixUpdate fixedUpdate r 0  
  | otherwise = swapAndVerifyUpdate u r i (j + 1)
  where fixedUpdate = swapElement u i j 

swapAndVerifyUpdateWithMoreThanOneError :: Update -> [PageOrderingRule] -> Int -> Update
swapAndVerifyUpdateWithMoreThanOneError u r i 
  | firstValidIndex /= Nothing = allUpdateConfigs !! (fromMaybe 0 firstValidIndex)
  | firstValidIndex == Nothing = fixUpdate allUpdateConfigs -- THE PROBLEM IS HERE 
  where allUpdateConfigs = map (\j -> swapElement u i j) ([0..(i-1)] ++ [(i+1)..(length u - 1)])
        isValidList = zip (map (\x -> verifyUpdate x r 0) allUpdateConfigs) [0..]
        firstValidIndex = findFirstTrue isValidList

findFirstTrue :: [(Bool, Int)] -> Maybe Int
findFirstTrue pairs = fmap snd $ find fst pairs
  where
    find _ [] = Nothing
    find predicate (x:xs)
      | predicate x = Just x
      | otherwise   = find predicate xs



-- FROM STACKOVERFLOW -- https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices
swapElement :: [a] -> Int -> Int -> [a]
swapElement lst i j
    | i < 0 || i >= length lst || j < 0 || j >= length lst = error "Index out of bounds"
    | otherwise = [if x == i then lst !! j else if x == j then lst !! i else lst !! x | x <- [0..length lst - 1]]
-- END FROM STACKOVERFLOW --
                     
verifyUpdate :: Update -> [PageOrderingRule] -> Int -> Bool
verifyUpdate u r i
  | i == (length u) - 1 = True
  | elem updatePairs r == True = verifyUpdate u r (i + 1)
  | otherwise = False
  where updatePairs = (u !! i, u !! (i + 1))
  

convertToPageOrderingRule :: String -> PageOrderingRule
convertToPageOrderingRule s = zipPair (map read (doRegex s regexSplitPipe))
  where regexSplitPipe = "[[:digit:]]+|[[:digit:]]+"

zipPair :: [Int] -> PageOrderingRule
zipPair [x, y] = (x, y)

convertToUpdate :: String -> Update
convertToUpdate s = map convertStringToInt (doRegex s regexSplitComma)
  where regexSplitComma = "[[:digit:]]+"

convertStringToInt :: String -> Int
convertStringToInt s = read s

doRegex :: String -> String -> [String]
doRegex str regex = getAllTextMatches (str =~ regex) :: [String]
