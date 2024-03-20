-- exercises.hs

import Data.Bool
import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True = [False, True]
eftBool True False = []
eftBool True True = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd GT GT = [GT]
eftOrd _ _ = []

eftInt :: Int -> Int -> [Int]
eftInt x y
    | y < x = []
    | x == y = [x]
    | x < y = x : (eftInt (x + 1) y)

eftChar :: Char -> Char -> [Char]
eftChar x y
    | y < x = []
    | x == y = [x]
    | x < y = x : (eftChar (succ x) y)

myWords :: [Char] -> [[Char]]
myWords [] = []
myWords x = takeWhile (/=' ') x : (myWords (drop 1 (dropWhile (/=' ') x)))

mySqr = [x ^ 2 | x <- [1..5]]
myCube = [x ^ 3 | x <- [1..5]]

tuples1 = [(x, y) | x <- mySqr, y <- mySqr]
tuples2 = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y < 50]
tuples2Len = length tuples2

itIsMystery :: String -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (x':xs') = (x, x') : zip' xs xs'

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (x':xs') = (f x x') : zipWith' f xs xs'

zip'' :: [a] -> [b] -> [(a,b)]
zip'' a b = zipWith' (\x -> \y -> (x, y)) a b

f xs = [toUpper x | x <- xs]

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

capitalize' :: String -> String
capitalize' [] = []
capitalize' (x:xs) = toUpper x : capitalize' xs

capitalize'' :: String -> Char
capitalize'' = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs)
    | x = True
    | otherwise = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr $ map f xs

myElem :: Eq a => a -> [a] -> Bool
myElem e xs = myAny (\x -> x == e) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain xs = squishMap (\x -> x) xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [] = undefined
myMaximumBy f (x:xs) = go f x xs where
    go _ x [] = x
    go f x (x':xs') = case f x (go f x' xs') of
        LT -> go f x' xs'
        _ -> x

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [] = undefined
myMinimumBy f (x:xs) = go f x xs where
    go _ x [] = x
    go f x (x':xs') = case f x (go f x' xs') of
        GT -> go f x' xs'
        _ -> x

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs
