{-
  If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
  If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

  Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
-}

(|>) = flip ($)

multiplesOfTen = ["", " ten", " twenty", " thirty", " forty", " fifty", " sixty", " seventy", " eighty", " ninety"]
others = ["", " one", " two", " three", " four", " five", " six", " seven", " eight", " nine", " ten", " eleven", " twelve", " thirteen", " fourteen", " fifteen", " sixteen", " seventeen", " eighteen", " nineteen"]

convert :: Int -> String
convert x 
  | x == 0 = ""
  | x `mod` 100 < 20 = (convert (x `div` 100)) ++ others !! (x `mod` 100)
  | otherwise = (convert (x `div` 10)) ++ multiplesOfTen !! (x `mod` 10) ++ others !! (x `mod` 10)