import Data.List
import Data.Maybe
import Data.Char


occurs :: Eq a => a -> [a] -> Int
occurs c []     = 0
occurs c xs = length [x | x <- xs, c == x ]

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

elementAt'' ::  [a] -> Int -> a
elementAt'' (x:_) 1  = x
elementAt'' (_:xs) i = elementAt'' xs (i - 1)
elementAt'' _ _      = error "Index out of bounds"

findValidPasswordsPart1 :: [(String,String,String,String)] -> Int
findValidPasswordsPart1 xs = length [occurs (head c) s |(lb,ub,c,s) <- xs, (occurs (head c) s) <= (read ub :: Int) && (occurs (head c) s) >= (read lb :: Int) ]

findValidPasswordsPart2 :: [(String,String,String,String)] -> Int
findValidPasswordsPart2 xs = length $ filter (==True) [((elementAt'' s (read ub :: Int)) == (head c)) `xor'` ((elementAt'' s (read lb :: Int)) == (head c)) | (lb,ub,c,s) <- xs]

zip4Elements :: [String] -> [(String,String,String,String)]
zip4Elements [] = [(" ", " ", " ", " ")]
zip4Elements (x:y:z:s:xs) = [(x,y,z,s)] ++ zip4Elements xs --[(take 4 xs)] ++ zip4Elements xs


inputFileToList :: IO()
inputFileToList = do
  n <- readFile "input.txt"
  let b = words $ n
  let c = drop 1 $ reverse (zip4Elements b)
  let d = findValidPasswordsPart2 c
  putStrLn (show d)
