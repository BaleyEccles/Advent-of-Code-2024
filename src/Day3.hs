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
  print program
  let mulRegex = "mul\\([[:digit:]]+,[[:digit:]]+\\)"
  let outputMul = getAllTextMatches (program =~ mulRegex) :: [String]
  print outputMul
  let numRegex = "[[:digit:]]+"
  let outputNum = concatMap (\x -> [getAllTextMatches (x =~ numRegex) :: [String]]) outputMul
  print outputNum
  let intVal = map toPair outputNum
  print intVal
  let multSum = sum ((map multiply) intVal)
  print "Solution to part 1:" 
  print multSum

  -- Part 2
  let mulEnableRegex = "(mul\\([[:digit:]]+,[[:digit:]]+\\))|(don't\\(\\))|(do\\(\\))"
  let outputMulEnable = getAllTextMatches (program =~ mulEnableRegex) :: [String]
  print outputMulEnable
  let mulEnableBool = map toBool outputMulEnable


  print mulEnableBool
  let removedMuls = removeMuls mulEnableBool
  print removedMuls
  let mulEnableBoolRemovedTrue = filter removeTrue removedMuls
  print mulEnableBoolRemovedTrue

  let outputNum2 = concatMap (\x -> [getAllTextMatches (x =~ numRegex) :: [String]]) mulEnableBoolRemovedTrue
  print outputNum2
  let intVal2 = map toPair outputNum2
  print intVal2
  let multSum2 = sum ((map multiply) intVal2)
  print "Solution to part 2:" 
  print multSum2

  hClose handle

removeTrue :: String -> Bool
removeTrue s
  | s == "True" = False
  | otherwise = True
  

removeMuls :: [String] -> [String]
removeMuls [] = []  -- Base case: if the list is empty, return an empty list
removeMuls (x:xs)
  -- Case: "True", "mul"
  | x == "True"  && (head xs /= "False") = 
      x : removeMuls xs
  -- Case: "True", "False"
  | x == "True"  && (head xs == "False") = 
      removeMuls xs
  -- Case: "False", "True"
  | x == "False"  && (head xs == "True") = 
      removeMuls xs
  -- Case: "False", "False"
  | x == "False"  && (head xs == "False") = 
      removeMuls xs
  -- Case: "False", "mul"
  | x == "False"  && (head xs /= "True") = 
      removeMuls (x : (tail xs))
  -- Case: "mul"
  | x /= "False" && x /= "True" =
      x : removeMuls xs
  | otherwise = removeMuls xs
  
multiply :: (Int, Int) -> Int
multiply a = (fst a)*(snd a)

toPair :: [String] -> (Int, Int)
toPair [x, y] = (read x, read y)

toBool :: String -> String
toBool a
  | a == "do()" = "True"
  | a == "don't()" = "False"
  | otherwise = a
  
  
