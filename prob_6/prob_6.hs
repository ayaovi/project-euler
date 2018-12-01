-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
main :: IO()
main = do
  let s = foldl (+) 0 [x * x | x <- [1..100]]
  let s' = foldl (+) 0 [1..100]
  let result = (-) (s' * s') s
  print result