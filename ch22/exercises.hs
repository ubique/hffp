-- exercises.hs

module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' x xs = foldr (\a b -> if fst a == x then Just (snd a) else b) Nothing xs

xs :: Maybe Integer
xs = lookup' 3 $ zip x y

ys :: Maybe Integer
ys = lookup' 6 $ zip y z

zs :: Maybe Integer
zs = lookup' 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup' n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (,) (z' n) (z' n)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f t = f (fst t) (snd t)

summed :: Num c => (c, c) -> c
summed = uncurry' (+)

bolt :: Integer -> Bool
bolt x = x <= 7

fromMaybe' :: a -> Maybe a -> a
fromMaybe' v Nothing = v
fromMaybe' _ (Just v) = v

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
    print $ sequenceA [Just 3, Just 2, Just 1]

    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ bolt 7
    print $ fmap bolt z

    print $ sequenceA [(>3), (<8), even] 7

    print $ foldr (&&) False $ sequA 7
    print $ sequA (fromMaybe' 0 s')
    print $ bolt (fromMaybe' 0 ys)
