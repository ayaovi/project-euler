-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
sum_of_square = foldl (+) $ map (\x -> x * x) [1..100]
s = foldl (+) 0 [1..100]
result = (-) (s * s) sum_of_square
