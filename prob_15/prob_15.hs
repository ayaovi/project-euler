{-
  Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
  How many such routes are there through a 20×20 grid?
-}

import Control.Parallel

(|>) = flip ($)

cells = 11
size = cells + 1

moves :: Int -> Int -> [[Int]]
moves x y 
  | (x `mod` size == cells) && (down > y) = [[x]]
  | (x `mod` size == cells) && (down <= y) = (moves down y) |> map ((:) x)
  | (x `mod` size < cells) && (down > y) = (moves right y) |> map ((:) x)
  | otherwise = [moves right y, moves down y] |> map (\f -> f `par` map ((:) x) f) |> foldl (++) []
  where right = x + 1
        down = x + size

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

result = (factorial 40) `div` (factorial 20 * factorial 20)