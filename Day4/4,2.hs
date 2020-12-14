import Data.List
import Data.Maybe
import Data.Char
import Data.List.Split


-- break the whols list of lists into a list if lists of tuples
listOfListOfTuples :: [[String]] -> [[(String, String)]]
listOfListOfTuples [x] = [listOfTuples x]
listOfListOfTuples (x:xs) = [listOfTuples x] ++ listOfListOfTuples xs

listValid1 = ["eyr","cid", "hcl", "ecl", "hgt", "pid", "iyr", "byr"]
listValid2 = ["eyr", "hcl", "ecl", "hgt", "pid", "iyr", "byr"]

getAllObligatoryFields xs | sort [a | (a,b) <- xs] == sort listValid1 = validatePassports xs
                          | sort [a | (a,b) <- xs] == sort listValid2 = validatePassports xs
                          | otherwise = [False]

listOfTuples :: [String] -> [(String, String)]
listOfTuples [] = []
listOfTuples (xs) = map (\x -> (take 3 x, takeWhile (/=' ') $ drop 4 x)) xs

validatePassports :: [(String,String)] -> [Bool]
validatePassports [] = []
validatePassports (x) =   [check (a,b) | (a,b) <- x ]

check (a,b)  | "hgt" `isPrefixOf` a = validateHeight (a,b)
             |  otherwise = boolValidate (a,b)


validateHeight (a,b)  | "cm" `isPrefixOf` (reverse $ take 2 $ reverse b) =  boolValidate ((reverse $ take 2 $ reverse b), takeWhile (/='c') b)
                      | "in" `isPrefixOf` (reverse $ take 2 $ reverse b) =  boolValidate ((reverse $ take 2 $ reverse b), takeWhile (/= 'i') b)
                      | otherwise =  boolValidate (a,b)

validateHeight2 (a,b) = ((reverse $ take 2 $ reverse b), takeWhile (/='c') b)

boolValidate :: (String,String) -> Bool-- most                   -- least
boolValidate (a,b) = case a of "byr" ->   ((read b :: Int) `elem` [1920..2002]) && length b == 4
                               "iyr" ->   ((read b :: Int) `elem` [2010..2020]) && length b == 4
                               "eyr" ->   ((read b :: Int)`elem` [2020..2030]) && (length b == 4)
                               "cm"  ->   ((read b :: Int) `elem` [150..193])
                               "in"  ->   ((read b :: Int) `elem` [59..76])
                               "ecl"  ->  b  `elem` ["amb", "blu", "brn", "gry", "grn", "hzl","oth" ]
                               "pid"  ->  length b == 9 && and [isDigit x |x <- b] &&  and [x  `elem` "0123456789" |x <- b]
                               "hcl" ->   length b == 7 && "#" `isPrefixOf` b && (and [x `elem` "0123456789abcdef" | x <- (drop 1 b)])
                               "cid" ->   True
                               _    ->    False



inputFileToList :: IO()
inputFileToList = do
  n <- readFile "input.txt"
  let b = unwords (lines n)
  let c = splitOn "  " b
  let d = map words c
  let e = listOfListOfTuples d
  let validate = map getAllObligatoryFields e
  let f = map and validate
  let g = length $ filter (==True) f
  putStrLn (show g)
