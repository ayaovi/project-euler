{-
  The following iterative sequence is defined for the set of positive integers:

  n → n/2 (n is even)
  n → 3n + 1 (n is odd)

  Using the rule above and starting with 13, we generate the following sequence:
  13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
  It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

  Which starting number, under one million, produces the longest chain?

  NOTE: Once the chain starts the terms are allowed to go above one million.
-}

import Data.List
import Control.Parallel

(|>) = flip ($)

collatz :: Int -> [Int]
collatz 0 = []
collatz 1 = [1]
collatz x 
  | even x = x:collatz (x `div` 2)
  | odd x = x:collatz (x * 3 + 1)

collatzr :: Int -> Int -> [Int]  -- gen collatz of every number in range and returns the longest chain.
collatzr x y = [x..y] |> map (\x -> collatz x) |> maximumBy (\x y -> compare (length x) (length y))

workers = 8
bucket = 1000000 `div` workers -- workerd needs to be a divisor of 1000000

main :: IO()
main  = do 
  let res = [1..workers] |> map (\x -> ((x - 1) * bucket, x * bucket)) |> map (\(x, y) -> collatzr x y) |> map (\x -> x `par` x) |> maximumBy (\x y -> compare (length x) (length y))
  print res