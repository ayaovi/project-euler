{-
  Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
  How many such routes are there through a 20×20 grid?
-}

(|>) = flip ($)

grid_points = [0..399]

moves :: [[Int]]
moves = [0] : concatMap gen moves
  where gen xs
            | last xs == 8 = []
            | (right `mod` 3 == 2) && (down > 8) = []
            | (right `mod` 3 < 2) && (down > 8) = [last xs : right]
            | (right `mod` 3 > 2) && (down < 8) = [last xs : down]
            | otherwise = [last xs : right, last xs : down]
            where right = (last xs) + 1
                  down = (last xs) + 3 