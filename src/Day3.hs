module Day3 where

import System.IO  
import Data.List (sort)
import Data.List (group)
import Text.Regex.TDFA

day3 = do
  handle <- openFile "./input/Day3.txt" ReadMode
  program <- hGetContents handle
  let singleLines = lines program
  -- For testing
  --let program = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
  -- Part 1
  --print program
  let mulRegex = "mul\\([[:digit:]]+,[[:digit:]]+\\)"
  let multList = doRegex program mulRegex

  let numRegex = "[[:digit:]]+"
  let numPairList = concatMap (\x -> [doRegex x numRegex]) multList
  let intPairList = map toPair numPairList
  let multSum = sum ((map multiply) intPairList)
  print "Solution to part 1:" 
  print multSum

  -- Part 2
  let mulDoDontRegex = "(mul\\([[:digit:]]+,[[:digit:]]+\\))|(don't\\(\\))|(do\\(\\))"
  let multDoDontList = doRegex program mulDoDontRegex
  let multDoDontBool = map toBool multDoDontList

  let removedDontMuls = removeDontMuls multDoDontBool
  let multList2 = filter removeTrue removedDontMuls

  let numPairList2 = concatMap (\x -> [doRegex x numRegex]) multList2
  let intPairList2 = map toPair numPairList2
  let multSum2 = sum ((map multiply) intPairList2)
  print "Solution to part 2:" 
  print multSum2


  hClose handle

removeTrue :: String -> Bool
removeTrue s
  | s == "True" = False
  | otherwise = True
  

removeDontMuls :: [String] -> [String]
removeDontMuls [] = []  -- Base case: if the list is empty, return an empty list
removeDontMuls (x:xs)
  -- Case: "True", "mul"
  | x == "True"  && (head xs /= "False") = 
      x : removeDontMuls xs
  -- Case: "True", "False"
  | x == "True"  && (head xs == "False") = 
      removeDontMuls xs
  -- Case: "False", "True"
  | x == "False"  && (head xs == "True") = 
      removeDontMuls xs
  -- Case: "False", "False"
  | x == "False"  && (head xs == "False") = 
      removeDontMuls xs
  -- Case: "False", "mul"
  | x == "False"  && (head xs /= "True") = 
      removeDontMuls (x : (tail xs))
  -- Case: "mul"
  | x /= "False" && x /= "True" =
      x : removeDontMuls xs
  | otherwise = removeDontMuls xs
  
multiply :: (Int, Int) -> Int
multiply a = (fst a)*(snd a)

toPair :: [String] -> (Int, Int)
toPair [x, y] = (read x, read y)

toBool :: String -> String
toBool a
  | a == "do()" = "True"
  | a == "don't()" = "False"
  | otherwise = a
  
  
doRegex :: String -> String -> [String]
doRegex str regex = getAllTextMatches (str =~ regex) :: [String]
