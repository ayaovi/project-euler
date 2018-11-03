-- What is the largest prime factor of the number 600851475143 ?
prime_factors :: Int -> [Int]
prime_factors n =
  case factors of 
    [] -> [n]
    _  -> factors ++ prime_factors ((div) n (head factors))
  where factors = take 1 $ filter (\x -> (==) (mod n x) 0) [2 .. ceiling $ sqrt (fromIntegral (n - 1))]

result = last (prime_factors 600851475143)
