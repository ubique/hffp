-- WordNumber.hs

module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits 0 = [0]
digits n = reverse $ go n
    where go n
            | n == 0 = []
            | otherwise = [mod n 10] ++ go (div (n - mod n 10) 10)

wordNumber :: Int -> String
wordNumber n = concat (intersperse ['-'] (map digitToWord $ digits n))
