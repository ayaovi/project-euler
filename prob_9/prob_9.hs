{-
  A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
  a^2 + b^2 = c^2
  For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.
-}

is_pythagorean_triplet :: Int -> Int -> Int -> Bool
is_pythagorean_triplet x y z = x * x + y * y == z * z 

ab = head $ dropWhile (\(x, y) -> not $ is_pythagorean_triplet x y (1000 - x - y)) $ concat $ map (\x -> map (\y -> (x, y)) [1..1000]) [1..1000]
result = fst ab * snd ab * (1000 - fst ab - snd ab)