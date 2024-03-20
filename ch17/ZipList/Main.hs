module Main where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList' a = ZipList' { getZipList' :: [a] } deriving (Eq, Show)

instance Arbitrary1 ZipList' where
  liftArbitrary = fmap ZipList' . liftArbitrary
  liftShrink shr = map ZipList' . liftShrink shr . getZipList'

instance (Arbitrary a) => Arbitrary (ZipList' a) where
    arbitrary = arbitrary1

instance (CoArbitrary a) => CoArbitrary (ZipList' a) where
    coarbitrary (ZipList' []) = variant 0
    coarbitrary (ZipList' (x:xs)) = variant 1. coarbitrary (x,xs)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                    in take 3000 l
              ys' = let (ZipList' l) = ys
                    in take 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure a = ZipList' [a]
    (<*>) (ZipList' []) _ = ZipList' []
    (<*>) _ (ZipList' []) = ZipList' []
    (<*>) (ZipList' [f]) xs = fmap f xs
    (<*>) (ZipList' fs@(f:_)) (ZipList' [x]) = ZipList' (fmap ($ x) fs)
    (<*>) (ZipList' (f : fs)) (ZipList' (x : xs)) = ZipList' ((f x) : (append' fs xs)) where
        append' [] _ = []
        append' _ [] = []
        append' (f' : fs') (x' : xs') = (f' x') : append' fs' xs'

zl' = ZipList'
z = zl' [(+9), (*2), (+8)]
z' = zl' [1..3]
z'' = pure 1
z''' = zl' [1, 2]


main :: IO ()
main = do
        putStrLn "Hello, Haskell!"
        print (z <*> z')
        print (z <*> z'')
        print (pure id <*> z''')
        quickBatch (applicative [(ZipList' ["a"], ZipList' ["b"], ZipList' ["c"])])
