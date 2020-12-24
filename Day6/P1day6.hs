import Data.List
import Data.Maybe
import Data.Char
import Data.List.Split





inputFileToList :: IO()
inputFileToList = do
  n <- readFile "input.txt"
  let b =  unwords $ lines n
  let c = splitOn "  " b
  let d = map words c
  let f = map concat d
  let g = map nub f
  let h = map length g
  let i = sum h
  putStrLn (show i)
