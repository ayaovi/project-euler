{-
  2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
  What is the sum of the digits of the number 2^1000?
-}

import Data.Char
import Data.List

(|>) = flip ($)

fullMult :: Int -> Int -> Int
fullMult x y = y' |> map (\(v, w) -> (mult x' v) ++ (replicate w 0))
                  |> map (\z -> foldr (\v w -> (show v ++ w)) "" z)
                  |> map (\z -> read z)
                  |> sum
  where x' = [digitToInt z | z <- show x]
        y' = zip [digitToInt z | z <- show y] (reverse [0..length (show y) - 1])

fullMult' :: String -> String -> String
fullMult' x y = y' |> map (\(v, w) -> (mult x' v) ++ (replicate w 0))
                   |> map (\z -> foldr (\v w -> (show v ++ w)) "" z)
                   |> map (\s -> trim s '0')
                   |> flip (pad) '0'
                   |> add
                   |> flip (trim) '0'
  where x' = [digitToInt z | z <- x]
        y' = zip [digitToInt z | z <- y] (reverse [0..length y - 1])

pad :: [String] -> Char -> [String]
pad xs c = map (\x -> (replicate (ml - length x) c) ++ x) xs
  where ml = xs |> map (\x -> length x) |> maximum

trim :: String -> Char -> String
trim "" _ = ""
trim (x:xs) c 
  | x == c = trim xs c
  | otherwise = (x:xs)

add :: [String] -> String
add d =
  let p = transpose [[digitToInt y | y <- x] | x <- d]

      halfAdd (d1, c1) d2 = let r = 10
                                s = d1 + d2 + c1 * r
                            in  (s `mod` r, s `div` r)
      p' = map (foldl halfAdd (0, 0)) p

      digit (lv, lc) (rv, rc) = 
        let (nv, nc) = halfAdd (lv, lc) rc 
        in ((show nv) ++ rv, nc)

      (acc, rem) = foldr digit ([], 0) p'
  in  show rem ++ acc

mult :: [Int] -> Int -> [Int]
mult x y = (rem:acc)
  where (acc, rem) = foldr digits ([], 0) z
        z = map (\d -> halfMult (d, 0) y) x
        digits (lv, lc) (rv, rc) = ((s `mod` r):rv, (s `div` r) + lc)
          where s = lv + rc
                r = 10

halfMult :: (Int, Int) -> Int -> (Int, Int)
halfMult (d1, c1) d2 = (s `mod` r, s `div` r)
  where s = d1 * d2 + c1
        r = 10

main :: IO()
main = do
  let res = (replicate 1000 2) |> map (show) 
                         |> foldl (fullMult') "1"
                         |> map (digitToInt)
                         |> foldl (+) 0
                         
  let res' = (sum . map (digitToInt) . show) (2 ^ 1000)
  
  print res'