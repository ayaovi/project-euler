-- Find the sum of all the primes below two million.
is_prime :: Int -> Bool
is_prime n = (==) [] $ filter (\x -> (==) (mod n x) 0) [2 .. ceiling $ sqrt (fromIntegral (n - 1))]

-- primes :: [Int] -> [Int]
-- primes [] = []
-- primes (x:xs) = x : primes (filter (\x' -> (/=) (x' `mod` x) 0) xs)

result = foldl (+) 2 $ takeWhile (\x -> (>) 2000000 x) $ filter (\x -> is_prime x) [3,5..]
-- result = foldl (+) 2 $ primes [3,5..2000000]