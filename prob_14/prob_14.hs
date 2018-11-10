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

collatzr :: Int -> Int -> [Int]  -- generates collatz for every number in the specified range and returns the longest chain.
collatzr x y = [x..y] |> map (\x -> collatz x) |> maximumBy (\x y -> compare (length x) (length y))

res = s `par` t `par` u `par` v `par` w `par` x `par` y `par` z `pseq` [s, t, u, v, w, x, y, z] |> maximumBy (\x y -> compare (length x) (length y))
  where s = collatzr 1 125000
        t = collatzr 125001 250000
        u = collatzr 250001 375000
        v = collatzr 375001 500000
        w = collatzr 500001 625000
        x = collatzr 625001 750000
        y = collatzr 750001 875000
        z = collatzr 875001 1000000