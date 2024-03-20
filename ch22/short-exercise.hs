-- short-exercise.hs

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = reverse . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tuppled :: [Char] -> ([Char], [Char])
tuppled = (,) <$> cap <*> rev


tuppled' :: [Char] -> ([Char], [Char])
tuppled' = do
    a <- cap
    b <- rev
    return (a, b)
