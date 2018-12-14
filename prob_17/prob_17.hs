{-
  If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
  If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

  Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
-}

(|>) = flip ($)

multiplesOfTen = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
oneToTwenty = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

convert :: Int -> Bool -> String
convert x f
  | x == 0 = ""
  | x >= 1000 = (oneToTwenty !! (x `div` 1000)) ++ "thousand" ++ (if (x `mod` 1000) > 100 then convert (x `mod` 1000) False else convert (x `mod` 1000) True)
  | x >= 100 = (oneToTwenty !! (x `div` 100)) ++ "hundred" ++ (convert (x `mod` 100) True)
  | x `mod` 100 < 20 = (if f then "and" else "") ++ (convert (x `div` 100) False) ++ oneToTwenty !! (x `mod` 100)
  | otherwise = (if f then "and" else "") ++ multiplesOfTen !! (x `div` 10) ++ oneToTwenty !! (x `mod` 10)

main :: IO()
main = do
  let x = [convert i False | i <- [1..1000]] |> foldl (++) "" |> length
  print x