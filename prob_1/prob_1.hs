-- Find the sum of all the multiples of 3 or 5 below 1000.
result = foldl (+) 0 $ filter (\c -> (==) (mod c 3) 0 || (==) (mod c 5) 0) [1..999]
