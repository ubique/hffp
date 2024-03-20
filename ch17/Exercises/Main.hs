module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative (liftA3)

-- 17.9 Chapter exercises
-- 1.
-- Type []
-- pure :: a -> [a]
-- :t pure 'c' :: [] Char
-- pure 'c' :: [] Char :: [Char]
-- :t pure :: Char -> [] Char
--    pure :: Char -> [] Char :: Char -> [Char]
-- <*> :: [] (a -> b) -> [a] -> [b]
-- Prelude> :t (<*>) :: Maybe (Char -> Char) ->  Maybe Char -> Maybe Char
-- (<*>) :: Maybe (Char -> Char) ->  Maybe Char -> Maybe Char :: Maybe (Char -> Char) -> Maybe Char -> Maybe Char

-- 2. IO
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b
-- Prelude> :t pure :: Char -> IO Char
--          pure :: Char -> IO Char :: Char -> IO Char
-- Prelude> :t (<*>) :: IO (Char -> [Char]) -> IO Char -> IO [Char]
--          (<*>) :: IO (Char -> [Char]) -> IO Char -> IO [Char] :: IO (Char -> [Char]) -> IO Char -> IO [Char]

-- 3. Type (,) a
-- pure b :: (,) a -> (,) a b
-- (<*>) :: (,) a (b -> c) -> (a, b) -> (a, c)

-- Prelude> :t pure :: [Char] -> (,) [Integer] [Char]
-- pure :: [Char] -> (,) [Integer] [Char] :: [Char] -> ([Integer], [Char])
-- Prelude> :t (<*>) :: (,) [Integer] (Char -> [Char]) -> (,) [Integer] Char -> (,) [Integer] [Char]
-- (<*>) :: (,) [Integer] (Char -> [Char]) -> (,) [Integer] Char -> (,) [Integer] [Char] :: ([Integer], Char -> [Char]) -> ([Integer], Char) -> ([Integer], [Char])

-- 4. Type (->) e
-- pure :: (e -> b) -> f (e -> b)
-- (<*>) :: f (e -> b) -> f e -> f b
-- Prelude> :t pure :: (Char -> Char) -> Maybe (Char -> Char)
-- pure :: (Char -> Char) -> Maybe (Char -> Char) :: (Char -> Char) -> Maybe (Char -> Char)
-- Prelude> :t (<*>) :: Maybe (Char -> [Char]) -> Maybe Char -> Maybe [Char]
-- (<*>) :: Maybe (Char -> [Char]) -> Maybe Char -> Maybe [Char] :: Maybe (Char -> [Char]) -> Maybe Char -> Maybe [Char]

-- 1. data Pair a = Pair a a deriving Show
data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    pure a = Pair a a
    (<*>) (Pair x y) (Pair x' y') = Pair (x x') (y y')

instance (Eq a) => EqProp (Pair a) where
    (=-=) (Pair x y) (Pair x' y') = (x == x') .&&. (y == y')

pairGen :: (Arbitrary a) => Gen (Pair a)
pairGen = do
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = pairGen

instance (CoArbitrary a) => CoArbitrary (Pair a) where
    coarbitrary (Pair x y) = coarbitrary x . coarbitrary y

-- 2. This should look familiar:
--    data Two a b = Two a b
data Two a b = Two a b deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    (<*>) (Two a b) (Two a' b') = Two (a <> a') (b b')

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) (Two x y) (Two x' y') = (x == x') .&&. (y == y')

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = twoGen

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (Two a b) where
    coarbitrary (Two x y) = coarbitrary x . coarbitrary y

-- 3. data Three a b c = Three a b c
data Three a b c = Three a b c deriving Show

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty
    mappend = (<>)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    (<*>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c c')

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) (Three x y z) (Three x' y' z') = (x == x') .&&. (y == y') .&&. (z == z')

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = threeGen

instance (CoArbitrary a, CoArbitrary b, CoArbitrary c) => CoArbitrary (Three a b c) where
    coarbitrary (Three x y z) = coarbitrary x . coarbitrary y . coarbitrary z

-- 4. data Three' a b = Three' a b b
data Three' a b = Three' a b b deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Three' a b) where
    (<>) (Three' x y z) (Three' x' y' z') = Three' (x <> x') (y <> y') (z <> z')

instance (Monoid a, Monoid b) => Monoid (Three' a b) where
    mempty = Three' mempty mempty mempty
    mappend = (<>)

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Monoid a) => Applicative (Three' a) where
    pure x = Three' mempty x x
    (<*>) (Three' x y z) (Three' x' y' z') = Three' (x <> x') (y y') (z z')

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) (Three' x y z) (Three' x' y' z') = (x == x') .&&. (y == y') .&&. (z == z')

three'Gen :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
three'Gen = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three' x y z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = three'Gen

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (Three' a b) where
    coarbitrary (Three' x y z) = coarbitrary x . coarbitrary y . coarbitrary z

-- 5. data Four a b c d = Four a b c d
data Four a b c d = Four a b c d deriving Show

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (<>) (Four w x y z) (Four w' x' y' z') = Four (w <> w') (x <> x') (y <> y') (z <> z')

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
    mempty = Four mempty mempty mempty mempty
    mappend = (<>)

instance Functor (Four a b c) where
    fmap f (Four w x y z) = Four w x y (f z)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    (<*>) (Four w x y z) (Four w' x' y' z') = Four (w <> w') (x <> x') (y <> y') (z z')

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) (Four w x y z) (Four w' x' y' z') = (w == w') .&&. (x == x') .&&. (y == y') .&&. (z == z')

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four w x y z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = fourGen

instance (CoArbitrary a, CoArbitrary b, CoArbitrary c, CoArbitrary d) => CoArbitrary (Four a b c d) where
    coarbitrary (Four w x y z) =  coarbitrary w . coarbitrary x . coarbitrary y . coarbitrary z

-- 6. data Four' a b = Four' a a a b
data Four' a b = Four' a a a b deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Four' a b) where
    (<>) (Four' w x y z) (Four' w' x' y' z') = Four' (w <> w') (x <> x') (y <> y') (z <> z')

instance (Monoid a, Monoid b) => Monoid (Four' a b) where
    mempty = Four' mempty mempty mempty mempty
    mappend = (<>)

instance Functor (Four' a) where
    fmap f (Four' w x y z') = Four' w x y (f z')

instance (Monoid a) => Applicative (Four' a) where
    pure = Four' mempty mempty mempty
    (<*>) (Four' w x y z) (Four' w' x' y' z') = Four' (w <> w') (x <> x') (y <> y') (z z')

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) (Four' w x y z) (Four' w' x' y' z') = (w == w') .&&. (x == x') .&&. (y == y') .&&. (z == z')

four'Gen :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
four'Gen = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four' w x y z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = four'Gen

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (Four' a b) where
    coarbitrary (Four' w x y z) =  coarbitrary w . coarbitrary x . coarbitrary y . coarbitrary z

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos as bs cs = [(a, b, c) | a <- as, b <- bs, c <- cs]

combos' :: [a] -> [b] -> [c] -> [(a, b, c)]
combos' as bs cs = liftA3 (\x -> \y -> \z -> (x, y, z)) as bs cs

main :: IO ()
main = do
        putStrLn "Hello, Haskell!"
        quickBatch (applicative [(Four' 'a' 'a' 'a' False, Four' 'b' 'b' 'b' False, Four' 'c' 'c' 'c' False)])
        quickBatch (applicative [(Four "a" 'a' ["a"] False, Four "b" 'b' ["b"] False, Four "c" 'c' ["c"] False)])
        quickBatch (applicative [(Three' "a" 'a' 'x', Three' "b" 'b' 'y', Three' "c" 'c' 'z')])
        quickBatch (applicative [(Three "a" 'a' ["a"], Three "b" 'b' ["b"], Three "c" 'c' ["c"])])
        quickBatch (applicative [(Pair "a" "a", Pair "b" "b", Pair "c" "c")])
        quickBatch (applicative [(Two "a" 'a', Two "b" 'b', Two "c" 'c')])
