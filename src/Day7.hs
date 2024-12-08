module Day7 where

import System.IO  
import Data.List (find)
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA

type Equation = (Int, [Int])
type Operator = String

day7 = do
  handle <- openFile "./input/Day7.txt" ReadMode
  file <- hGetContents handle
  -- For Testing
  --let file = "190: 10 19 \n 3267: 81 40 27 \n 83: 17 5 \n 156: 15 6 \n 7290: 6 8 6 15 \n 161011: 16 10 13 \n 192: 17 8 14 \n 21037: 9 7 18 13 \n 292: 11 6 16 20"
  let equations = map convertToEquation (lines file)
  --print equations
  -- Part 1

  let calibration = findCalibration equations
  print calibration



findCalibration :: [Equation] -> Int
findCalibration e = sum (map validateEquation e)

validateEquation :: Equation -> Int
validateEquation e
  | equationIsValid e == True = solution
  | equationIsValid e == False = 0
    where solution = fst e


equationIsValid :: Equation -> Bool
equationIsValid e = (fst e) `elem` (generateAllPossibleSolutionsBinary (snd e) 0 [])

-- We can use binary numbers to get all the configs 0 -> 2^((length n) - 1) 
-- Where 0 is + and 1 is *
generateAllPossibleSolutionsBinary :: [Int] -> Int -> [Int] -> [Int]
generateAllPossibleSolutionsBinary n i list
  | i == max = list
  | otherwise = generateAllPossibleSolutionsBinary n (i + 1) (list ++ [currentResult])
    where len = length n
          max = 2^(len - 1)
          currentResult = applyMultAddList n ((intToBoolList (i)))


-- We can use base 3 numbers to get all the configs 0 -> 3^((length n) - 1) 
-- Where 0 is + and 1 is *
generateAllPossibleSolutionsThree :: [Int] -> Int -> [Int] -> [Int]
generateAllPossibleSolutionsThree n i list
  | i == max = list
  | otherwise = generateAllPossibleSolutionsThree n (i + 1) (list ++ [currentResult])
    where len = length n
          max = 23^(len - 1)
          currentResult = applyMultAddConcatList n ((intToOperatorList (i)))
          

applyMultAddConcatList :: [Int] -> [Operator] -> Int
applyMultAddConcatList ns bs
  | length ns == 1 = head ns
  | otherwise = applyMultAddList ((doAddOrMultOrConcat currentOperator n1 n2):nextEquations) (nextOperators)
  where nextEquations = tail (tail ns)
        nextOperators = if null bs then [] else tail bs
        currentOperator = if null (bs) then False else head (bs) 
        n1 = head ns
        n2 = if null (tail ns) then 0 else head (tail ns) 

doAddOrMultOrConcat :: Operator -> Int -> Int -> Int  
doAddOrMultOrConcat c a b
  | c == "+" = a + b
  | c == "*" = a * b
  | c == "||" = concatNumbers a b

concatNumbers :: Int -> Int -> Int
concatNumbers x y = read (show x ++ show y) :: Int

applyMultAddList :: [Int] -> [Bool] -> Int
applyMultAddList ns bs
  | length ns == 1 = head ns
  | otherwise = applyMultAddList ((doAddOrMult currentBool n1 n2):nextEquations) (nextBools)
  where nextEquations = tail (tail ns)
        nextBools = if null bs then [] else tail bs
        currentBool = if null (bs) then False else head (bs) 
        n1 = head ns
        n2 = if null (tail ns) then 0 else head (tail ns) 
    
 
  
doAddOrMult :: Bool -> Int -> Int -> Int  
doAddOrMult c a b
  | c == False = a + b
  | c == True = a * b

-- TODO
--intToOperatorList :: Int -> [Operator]
--intToOperatorList n

-- FROM CHATGPT
intToBoolList :: Int -> [Bool]
intToBoolList n
  | n == 0    = [False]  -- Return [False] for input 0
  | otherwise = map (== '1') (binaryRepresentation n)
  where
    binaryRepresentation 0 = "0"
    binaryRepresentation x = reverse (toBinary x)
      where
        toBinary 0 = ""
        toBinary y = toBinary (y `div` 2) ++ [if y `mod` 2 == 0 then '0' else '1']
-- NOT FROM CHATGPT
  

  
convertToEquation :: String -> Equation
convertToEquation line = (toInt (numbers !! 0), map toInt (tail numbers))
  where numbers = (doRegex line "[[:digit:]]+")
  
toInt :: String -> Int
toInt s = read s



doRegex :: String -> String -> [String]
doRegex str regex = getAllTextMatches (str =~ regex) :: [String]
