-- What is the 10 001st prime number?
is_prime :: Int -> Bool
is_prime n = (==) [] $ filter (\x -> (==) (mod n x) 0) [2 .. ceiling $ sqrt (fromIntegral (n - 1))]

result = fst $ (filter (\(x,y) -> (==) y True) $ map (\x -> (x, is_prime x)) [1..]) !! 10001