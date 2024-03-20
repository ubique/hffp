-- WordNumber.hs

module WordNumber
    ( digitToWord
    , digits
    , wordNumber
    )  where

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
    where go i
            | i == 0 = []
            | otherwise = [mod i 10] ++ go (div (i - mod i 10) 10)

wordNumber :: Int -> String
wordNumber n = concat (intersperse ['-'] (map digitToWord $ digits n))
