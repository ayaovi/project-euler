{-
  Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
  How many such routes are there through a 20×20 grid?

   0- 1- 2- 3
   4- 5- 6- 7
   8- 9-10-11
  12-13-14-15
-}

import Control.Parallel

(|>) = flip ($)

cells = 10
size = cells + 1

moves :: Int -> Int -> [[Int]]
moves x y 
  | (x `mod` size == cells) && (down > y) = [[x]]
  | (x `mod` size == cells) && (down <= y) = (moves down y) |> map ((:) x)
  | (x `mod` size < cells) && (down > y) = (moves right y) |> map ((:) x)
  | otherwise = [moves right y, moves down y] |> map (\f -> f `par` map ((:) x) f) |> foldl (++) []
  where right = x + 1
        down = x + size

result = (moves 0 (size * size - 1)) |> length