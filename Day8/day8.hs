import Data.List
import Data.Maybe
import Data.Char
import Data.List.Split

a = ["acc -8","jmp  5","acc  0","acc  44","acc  42","jmp  324","acc -17","jmp  1","acc -17","acc  5","acc -13"]

c :: [(String,Int,Bool)]
c  = [("acc",3,False),("jmp",5,False),("acc",0,False),("acc",44,False),
      ("acc",42,False),("jmp",3,False),("acc",-4,False),("jmp",1,False),
      ("acc",-17,False),("acc",51,False),("acc",-13,False)]

listOfTuples :: [String] -> [(String, Int, Bool)]
listOfTuples [] = []
listOfTuples (xs) = map (\x ->  getInt x) xs
  where getInt x = (take 3 x, read ( drop 4  x) :: Int, False)


elementAt'' ::  [a] -> Int -> a
elementAt'' (x:_) 0  = x
elementAt'' (_:xs) i = elementAt'' xs (i - 1)
elementAt'' _ _      = error "Index out of bounds"

validateOperation :: [(String, Int, Bool)] -> Int
validateOperation [] = 0
validateOperation (a : xs) = case a of ("acc", b, c) -> if c == False then b + validateOperation xs else validateOperation []
                                       ("jmp", b, c) -> if c == False then validateOperation (func ("jmp",b,c) xs) else validateOperation []
                                       _     -> validateOperation xs



func :: (String, Int, Bool) -> [(String, Int, Bool)]  -> [(String, Int, Bool)]
func a [] = [a]
func (a, b, c) xs     | b >= 0 = snd $ splitAt ((fromJust (elemIndex (a,b,c) xs)) + b ) xs
                      | b < 0  = snd $ splitAt ((fromJust $ elemIndex (a,b,c) xs) + b) xs





inputFileToList :: IO()
inputFileToList = do
  n <- readFile "input.txt"
  let b =  lines n
  let c = listOfTuples b
  let d = validateOperation c
  putStrLn (show d)
