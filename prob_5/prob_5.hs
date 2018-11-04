-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

import Data.List
import Data.Maybe

pf :: Int -> [Int] -- pf: prime factors.
pf n =
  case factors of 
    [] -> [n]
    _  -> factors ++ pf ((div) n (head factors))
  where factors = take 1 $ filter (\x -> (==) (mod n x) 0) [2..ceiling $ sqrt (fromIntegral (n - 1))]

occurences :: Int -> [Int] -> Int
occurences x xs = (length . filter (== x)) xs

pfc :: [Int] -> [(Int, Int)] -- pfc: prime factor count.
pfc xs = nubBy (\x y -> ((==) (fst x) (fst y)) && ((==) (snd x) (snd y))) $ map (\x -> (x, occurences x xs)) xs

dfof :: [(Int, Int)] -> [(Int, Int)]  -- dfof: discard fewer occurence factors.
dfof xs = nubBy (\x y -> ((==) (fst x) (fst y)) && ((>=) (snd x) (snd y))) xs

sfbo :: [(Int, Int)] -> [(Int, Int)] -- sfbo: sort factors by occurence.
sfbo xs = sortBy (\(_,a) (_,b) -> compare b a) xs

result = foldl (\x (y, z) -> x * round (fromIntegral y ** fromIntegral z)) 1 $ dfof $ sfbo $ concat $ map (\x -> pfc (pf x)) [20, 19..2]