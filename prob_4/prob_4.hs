-- Find the largest palindrome made from the product of two 3-digit numbers.

import Data.List

is_palindrome :: Int -> Bool
is_palindrome x = (==) (show x) (reverse (show x))

find_palindromes :: Int -> [Int]
find_palindromes i
  | (==) i 300 = []
  | otherwise = (++) xs $ find_palindromes (i - 1)
  where xs = (filter (\n -> is_palindrome n) $ map ((\x y -> x * y) i) [i, i - 1..100])

result = head $ reverse $ sort $ find_palindromes 999

-- result = head $ reverse $ sort $ filter (\n -> is_palindrome n) $ nub $ concat $ map (\i -> map ((\x y -> x * y) i) a) a
{-
  https://wiki.haskell.org/How_to_read_Haskell
  https://bartoszmilewski.com/category/haskell/

  the ':' operator prepends a list element to an existing list.

  READING TYPE SIGNATURE.

  inc :: Num a => a -> a
  inc: is the variable.
  Num a =>: is the context.
  a -> a: is its type, namely a function that takes a number and returns a number.

  map :: (a -> b) -> [a] -> [b]
  'map' takes any function of type (a -> b) and yields a function that takes a list 
  of a's and produces a list of b's. As such 'map' is a higher order function, 
  because it takes a function as input.

  A CANONICAL IMPLEMENTATION OF foldl

  foldl :: (a -> b -> a) -> a -> [b] -> a
  foldl f z [] = z
  foldl f z (x:xs) = foldl f (f z x) xs
-}