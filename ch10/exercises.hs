-- exercises.hs

-- foldl (flip (*)) 1 [1..3]
-- foldl (flip (*)) 1 [1, 2, 3]
-- foldl (flip (*)) 1 (flip (*) 1 1) [2, 3]
-- foldl (flip (*)) 1 (flip (*) (flip (*) 1 1) 2) [3]
-- foldl (flip (*)) 1 (flip (*) (flip (*) (flip (*) 1 1) 2) 3)
-- foldl (flip (*)) 1 (flip (*) (flip (*) (1) 2) 3)
-- foldl (flip (*)) 1 (flip (*) (2) 3)
-- foldl (flip (*)) 1 (6)
-- 6

-- foldr const 0 "tacos"
-- const 't' (foldr const 0 "acos")
-- const 't' (const 'a' foldr const 0 "cos")
-- const 't' (const 'a' (const 'c' foldr const 0 "os"))
-- const 't' (const 'a' (const 'c' (const 'o' foldr const 0 "s")))
-- const 't' (const 'a' (const 'c' (const 'o' (const 's' foldr const 0 ""))))
-- const 't' (const 'a' (const 'c' (const 'o' (const 's' 0))))

-- foldl const 0 "tacos"
-- foldl const (const 0 't') "acos"
-- foldl const (const (const 0 't') 'a') "cos"
-- foldl const (const (const (const 0 't') 'a') 'c') "os"
-- foldl const (const (const (const (const 0 't') 'a') 'c') 'o') "s"
-- foldl const (const (const (const (const (const 0 't') 'a') 'c') 'o') s') ""
-- const (const (const (const (const (const 0 't') 'a') 'c') 'o') s'

stops = "pbtdkg"
vowels = "aeiou"

genWords :: String -> String -> [String]
genWords _ [] = []
genWords [] _ = []
genWords (s:ss) (v:vs) = ([s, v, s] : genWords [s] vs) ++ genWords ss (v:vs)

genWords' :: String -> String -> [String]
genWords' _ [] = []
genWords' [] _ = []
genWords' (s:ss) (v:vs) = ([s, v, s] : genWords [s] vs)

genSentences :: [String] -> [String] -> [String]
genSentences _ [] = []
genSentences [] _ = []
genSentences (n:ns) (v:vs) = concat [n, v, n] : ((genSentences [n] vs) ++ (genSentences ns (v:vs)))

seekritFunc x = div (sum (map length (words x))) (length (words x))
seekritFunc' x = fromIntegral ((sum (map length (words x)))) / fromIntegral (length (words x))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr (map f xs)

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr (\x y -> f(x) || y) False

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\x y -> y || x == e) False

myReverse :: [a] -> [a]
myReverse = foldr (\x y -> y ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f(x) : y) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> (if f(x) == True then [x] else []) ++ y) []

squish :: [[a]] -> [a]
squish = foldr (\x y -> x ++ y) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x y -> f(x) ++ y) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [] = undefined
myMaximumBy f (x:xs) = foldl (\x y -> if f x y == GT then x else y) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [] = undefined
myMinimumBy f (x:xs) = foldl (\x y -> if f x y == LT then x else y) x xs
