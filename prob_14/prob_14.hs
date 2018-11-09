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

(\>) = flip ($)

collatz :: Int -> [Int]
collatz 1 = [1]
collatz 0 = []
collatz x 
  | even x = x:collatz (x `div` 2)
  | odd x = x:collatz (x * 3 + 1)

actual = Data.List.sortBy (\a b-> compare (length b) (length a)) $ map collatz [1..100]

collatzE f = 2 * f
collatzO f = (f-1) `div` 3

col = [1] : map (\s -> nub $ filter (\x -> x > 0) $ concat $ map (\x -> [collatzE x, collatzO x]) s) col

col2 :: Int -> [[Int]]
col2 x = 
  let evenC = collatzE x
      oddC = collatzO x
  -- in if evenC > 1 && oddC > 1 then [x:[evenC], x:[oddC]]
  -- in filter (\x -> last x > 0) [x:[evenC], x:[oddC]]
  in map (\a -> (head (col2 a)) ++ [x]) $ filter (>1) [evenC, oddC]
     