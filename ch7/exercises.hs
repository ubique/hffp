-- exercises.hs

module Exercises where

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = snd (divMod (fst (divMod x 10)) 10)

hunsD x = d2
    where d = x `div` 100
          d2 = d `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y z = case z of
    True -> x
    False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y z
    | z = x
    | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f (a), c)

data Product a b = Product a b deriving (Eq, Show)

productUnpack :: Product a b -> (a, b)
productUnpack (Product x y) = (x, y)
