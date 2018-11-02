fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)
result = foldl (+) 0 $ filter (\c -> (==) (mod c 2) 0) $ takeWhile ((>) 4000000) fib