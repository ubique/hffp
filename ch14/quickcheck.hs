-- quickcheck.hs

module QuickCheck where

import Test.QuickCheck
import Data.List

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: (Fractional a, Num a) => a -> a
halfIdentity = (*2) . half

prop_half :: Float -> Bool
prop_half x = x == halfIdentity x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
                 where go _ status@(_, False) = status
                       go y (Nothing, t) = (Just y, t)
                       go y (Just x, t) = (Just y, x >= y)

prop_sort :: [Int] -> Bool
prop_sort = listOrdered . sort

plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

prop_assoc :: Int -> Int -> Int -> Bool
prop_assoc = plusAssociative

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

prop_comm :: Int -> Int -> Bool
prop_comm = plusCommutative

mulAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
mulAssociative x y z = x * (y * z) == (x * y) * z

prop_mul_assoc :: Int -> Int -> Int -> Bool
prop_mul_assoc = mulAssociative

mulCommutative :: (Num a, Eq a) => a -> a -> Bool
mulCommutative x y = x * y == y * x

prop_mul_comm :: Int -> Int -> Bool
prop_mul_comm = mulCommutative

prop_quot_rem :: Int -> Int -> Bool
prop_quot_rem _ 0 = True
prop_quot_rem x y = (quot x y) * y + (rem x y) == x

prop_div_mod :: Int -> Int -> Bool
prop_div_mod _ 0 = True
prop_div_mod x y = (div x y) * y + (mod x y) == x

prop_pow_assoc :: Int -> Int -> Int -> Bool
prop_pow_assoc x y z = x ^ (y ^ z) == (x ^ y) ^ z

prop_double_reverse :: [Int] -> Bool
prop_double_reverse x = x == reverse (reverse x)

prop_id_dollar :: Int -> Bool
prop_id_dollar x = id $ x == id x

main :: IO ()
main = do
      quickCheck prop_half
      quickCheck prop_sort
      quickCheck prop_assoc
      quickCheck prop_comm
      quickCheck prop_mul_assoc
      quickCheck prop_mul_comm
      quickCheck prop_quot_rem
      quickCheck prop_div_mod
--    quickCheck prop_pow_assoc
      quickCheck prop_double_reverse
      quickCheck prop_id_dollar


