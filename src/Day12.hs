module Day12 where

import System.IO  
import Data.List (nub, intersect, sortBy, group, find)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA
import Control.Parallel.Strategies (parList, rseq, parMap)

type Pos = (Int, Int)
type Plant = (String, Pos)

day12 = do
  handle <- openFile "./input/Day12.txt" ReadMode
  file <- hGetContents handle
  --For testing
  let file = "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE"
  let plantMap = convertToPlantMap (lines file)
  print plantMap
  let r = getRegions plantMap
  print r
  let plantRegions = removeDuplicates (map nub (r))
  --print plantRegions
  let areaAndPerimeter = map getAreaAndPerimeter plantRegions
  print areaAndPerimeter
  let price = calculatePrice areaAndPerimeter
  print price
  let dirs = [("R",(1,1)), ("R",(0,1)), ("R",(1,0)), ("V",(1,2)), ("R",(2,1))]
  let checked = [("R",(1,1)), ("R",(0,0)), ("R",(0,1)), ("R",(2,0)), ("R",(1,0)), ("R",(1,1))]

  let l = findNotInChecked dirs checked
  print l 




removeLengthMoreThanOne :: [Plant] -> Bool
removeLengthMoreThanOne p
  | length p > 1 = False
  | otherwise = True
    
calculatePrice :: [(Int,Int)] -> Int
calculatePrice ap = sum (map (\x -> (fst x) * (snd x)) ap)
 

getAreaAndPerimeter :: [Plant] -> (Int, Int)
getAreaAndPerimeter p = (area, perimeter)
  where area = length p
        perimeter = getPerimeter p

getPerimeter :: [Plant] -> Int
getPerimeter p = sum [o |
                      a <- [0..((length p) - 1)],
                      let o = permiHelper p (p !! a)]
  where permiHelper p a = 4 - (length (filter isTrue (getNeighbours p a)))

isTrue :: Bool -> Bool        
isTrue b = b


getNeighbours :: [Plant] -> Plant -> [Bool]
getNeighbours ps p2 = ([(getFromPlant ps upPlant)] ++ [(getFromPlant ps downPlant)] ++ [(getFromPlant ps leftPlant)] ++ [(getFromPlant ps rightPlant)])
    where x = fst (snd p2)
          y = snd (snd p2)
          upPlant = (fst p2, ((fst (snd p2)), (snd (snd p2)) - 1))
          downPlant = (fst p2, ((fst (snd p2)), (snd (snd p2)) + 1))
          leftPlant = (fst p2, ((fst (snd p2)) - 1, (snd (snd p2))))
          rightPlant = (fst p2, ((fst (snd p2)) + 1, (snd (snd p2))))


getFromPlant :: [Plant] -> Plant -> Bool
getFromPlant ps p1 = p1 `elem` ps
                       
removeDuplicates :: [[Plant]] -> [[Plant]]
removeDuplicates p = (nub (map (\x -> sortBy compareByY x) (nub (map (\x -> sortBy compareByX x) p)))) 


sortXY :: [Plant] -> [Plant]
sortXY p = (\x -> sortBy compareByY x) ((\x -> sortBy compareByX x) p)
compareByX :: Plant -> Plant -> Ordering
compareByX p1 p2 = comparing (fst . snd) p1 p2
compareByY :: Plant -> Plant -> Ordering
compareByY p1 p2 = comparing (snd . snd) p1 p2


getRegions :: [Plant] -> [[Plant]]
getRegions p = [o |
                x <- [0..maxX],
                y <- [0..maxY],
                let o = createRegion p [] x y 0]
  where maxY = findMaxY p
        maxX = findMaxX p
        


createRegion :: [Plant] -> [Plant] -> Int -> Int -> Int -> [Plant]
createRegion p acc x y i
  | currentPlant == ("NULL", (-10,-10)) = []
  | currentPlant `elem` acc = []
  | otherwise = [currentPlant] ++ (concatMap (\a -> createRegion p newAcc (fst (snd a)) (snd (snd a)) (i+1)) touchingCorrectPlants)
  where touchingCorrectPlants = filter (\x -> isSamePlant (fst currentPlant) x) (getTouchingPlants p x y)
        touchingCorrectPlants2 = findNotInChecked touchingCorrectPlants newAcc
 -- where touchingCorrectPlants = filter (\x -> isSamePlant (fst currentPlant) x) (getTouchingPlants p x y)
        currentIdx = x + (y * (maxX + 1))
        maxX = findMaxX p
        currentPlant = fromMaybe ("NULL",(-10,-10)) (getAtIdx p currentIdx)
        newAcc = (nub acc) ++ [currentPlant]

--createRegion :: [Plant] -> [Plant] -> Int -> Int -> Int -> [Plant]
--createRegion p acc x y i
--  |
--  where currentIdx = x + (y * (maxX + 1))
--        maxX = findMaxX p
--        currentPlant = fromMaybe ("NULL",(-10,-10)) (getAtIdx p currentIdx)
--        newAcc = (nub acc) ++ [currentPlant]


findNotInChecked :: [Plant] -> [Plant] -> [Plant]
findNotInChecked dirs checked = filter (`notElem` checked) dirs
  
removeNULL :: Plant -> Bool
removeNULL p
  | fst p == "NULL" = False
  | otherwise = True

isSamePlant :: String -> Plant -> Bool
isSamePlant str p
  | str == fst p = True
  | otherwise = False

getTouchingPlants :: [Plant] -> Int -> Int -> [Plant]
getTouchingPlants p x y = ([fromMaybe ("NULL",(-10,-10)) (getAtIdx p upIdx)] ++ [fromMaybe ("NULL",(-10,-10)) (getAtIdx p downIdx)] ++ [fromMaybe ("NULL",(-10,-10)) (getAtIdx p leftIdx)] ++ [fromMaybe ("NULL",(-10,-10)) (getAtIdx p rightIdx)])
    where maxY = findMaxY p
          maxX = findMaxX p
          upIdx = x + ((y - 1) * (maxX + 1))
          downIdx = x + ((y + 1) * (maxX + 1))
          leftIdx = (x - 1) + (y * (maxX + 1))
          rightIdx = (x + 1) + (y * (maxX + 1))
        

getAtIdx :: [Plant] -> Int -> Maybe Plant
getAtIdx p i
  | i < 0 || i >= length p = Nothing
  | otherwise = Just (p !! i)

findMaxX :: [Plant] -> Int
findMaxX p = maximum (map (\a -> fst (snd a)) p)
findMaxY :: [Plant] -> Int
findMaxY p = maximum (map (\a -> snd (snd a)) p)

convertToPlantMap :: [String] -> [Plant]
convertToPlantMap p = concatMap (\x -> x) [[(char : [], (x, y)) | (char, x) <- zip row [0..]] | (row, y) <- zip p [0..]]
