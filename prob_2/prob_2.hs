-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)
result = foldl (+) 0 $ filter (\c -> (==) (mod c 2) 0) $ takeWhile ((>) 4000000) fib
