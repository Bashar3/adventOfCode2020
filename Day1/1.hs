import Data.List

{- For the first part just take away (z) from teh function sumTO2020
  and also the t
  like this:

  sumTo2020 :: (Num a, Eq a) => [a] -> a -> a
  sumTo2020 xs e =  sum [n*s |(n,s) <- [head l]]
    where l = (filter (\(x,y) -> x+y == e) [ (x,y,z) | x <- xs, y <- xs])

-}

sumTo2020 :: (Num a, Eq a) => [a] -> a -> a
sumTo2020 xs e =  sum [n*s*t |(n,s,t) <- [head l]]
  where l = (filter (\(x,y,z) -> x+y+z == e) [ (x,y,z) | x <- xs, y <- xs, z <- xs ])

inputFileToList :: IO()
inputFileToList = do
  n <- readFile "input.txt"
  let b = map (read :: String -> Int) . words $ n
  let c = sumTo2020 b 2020
  putStrLn (show c)
