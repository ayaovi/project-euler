-- Find the sum of all the primes below two million.

import Data.List -- (\\)

is_prime :: Int -> Bool
is_prime n = (==) [] $ filter (\x -> (==) (mod n x) 0) [3, 5 .. ceiling $ sqrt (fromIntegral (n - 1))]
-- is_prime n = null [x | x <- [3, 5 .. ceiling $ sqrt (fromIntegral (n - 1))], n `mod` x == 0]

-- primesTo :: Int -> [Int]
-- primesTo m = sieve [2..m]
--              where 
--              sieve (x:xs) = x : sieve (xs \\ [x,x+x..m])
--              sieve [] = []

primes :: [Int] -> [Int]
primes [] = []
primes (x:xs) = x : primes (filter (\x' -> (/=) (x' `mod` x) 0) xs)

main :: IO()
main = do
  let result = foldl (+) 2 $ takeWhile (\x -> (>) 2000000 x) $ filter (\x -> is_prime x) [3,5..]
  print result
-- result = foldl (+) 0 $ primesTo 2000000
-- result = foldl (+) 2 $ primes [3,5..2000000]