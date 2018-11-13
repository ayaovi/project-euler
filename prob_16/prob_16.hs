{-
  2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
  What is the sum of the digits of the number 2^1000?
-}

(|>) = flip ($)

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

res = 2 ** 1000