import Data.List
import Data.Maybe
import Data.Char
import Data.List.Split



s = "FBFBBFFRLR"

f = [0..63]
b = [64..127]

elementAt'' ::  [a] -> Int -> a
elementAt'' (x:_) 1  = x
elementAt'' (_:xs) i = elementAt'' xs (i - 1)
elementAt'' _ _      = error "Index out of bounds"




continueTranslateColumn :: [Int] -> String -> [Int]
continueTranslateColumn [] cc   = []
continueTranslateColumn xs []   = []
continueTranslateColumn lis (c:cc) | c == 'B' =  (drop (length lis `div` 2) lis) ++ continueTranslateColumn (drop (length lis `div` 2) lis) cc
                                   | c == 'F' =  (take (length lis `div` 2) lis) ++ continueTranslateColumn (take (length lis `div` 2) lis) cc
                                   | otherwise = continueTranslateColumn lis cc




continueTranslateRow :: [Int] -> String -> [Int]
continueTranslateRow [] cc   = []
continueTranslateRow xs []   = []
continueTranslateRow xs (c:cc) = case c of 'R' -> (drop (length xs `div` 2) xs) ++ continueTranslateRow (drop (length xs `div` 2) xs) cc
                                           'L' -> (take (length xs `div` 2) xs) ++ continueTranslateRow (take (length xs `div` 2) xs) cc

combineBothFunctions :: [Int] -> [Int] -> [String] -> [(Int, Int)]
combineBothFunctions intsC intsR [] = []
combineBothFunctions intsC intsR (s:ss) = [(last $ continueTranslateColumn intsC (take 7 s) , last $ continueTranslateRow intsR (drop 7 s))] ++ combineBothFunctions intsC intsR ss

zipToColAndRow :: [String] -> [(String, String)]
zipToColAndRow xs = [(x,x) |x <- xs ]

convertToSeatID :: (Int,Int) -> Int
convertToSeatID (a,b) = a*8 +b

inputFileToList :: IO()
inputFileToList = do
  n <- readFile "input1.txt"
  let d = words n
  let c = combineBothFunctions [0..127] [0..7] d
  let f = map convertToSeatID c
  let g =  sort f
  let part1 = last g
  let part2 = [6..951] \\ g
  putStrLn (show ("part1 :"++(show part1),"part2 :"++(show part2)))
