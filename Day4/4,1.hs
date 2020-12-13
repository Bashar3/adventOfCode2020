import Data.List
import Data.Maybe
import Data.Char
import Data.List.Split


-- filter every list directly which is longer than 7
f1s (xs) = filter (\x -> length x >= 7) xs

-- determin the length of every list in the list
e2s xs   = map length xs

-- filter every list if it contains the cid
d3s (xs) = map (filter (\x -> "cid" `isPrefixOf` x )) xs

-- compare the zipped list and subtract the 2 numbers (len (rules) - cid if exists) 
compare2List :: [(Int,Int)] -> Int
compare2List xs = length $ filter (>=7)[x-y | (x,y) <- xs]

inputFileToList :: IO()
inputFileToList = do
  n <- readFile "input.txt"
  let b = unwords (lines n)
  let c = splitOn "  " b
  let d = map words c
  -- first list original length of list >= 7
  let f = f1s d
  -- second list gives if it has cid in it
  let g = d3s f
  -- count the number of rules
  let n = e2s f
  -- count the number of cid
  let m = e2s g
  let zipNM = compare2List (zip n m)
  putStrLn (show zipNM)
