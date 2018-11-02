fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)
result = foldl (+) 0 $ filter (\c -> c < 4000000 && (==) (mod c 2) 0) $ map (\c -> fib !! c) [0..33]