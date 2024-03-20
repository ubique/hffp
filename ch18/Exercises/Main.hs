module Main where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Show)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = frequency [(1, return NopeDotJpg)]

instance Eq a => EqProp (Nope a) where
    (=-=) _ _ = property True

data BahEither b a = PLeft a | PRight b deriving (Show)

instance Functor (BahEither b) where
    fmap f (PLeft a) =  PLeft (f a)
    fmap _ (PRight b) = PRight b

instance Applicative (BahEither b) where
    pure = PLeft
    (PRight b) <*> _ = PRight b
    (PLeft f)  <*> r = fmap f r

instance Monad (BahEither b) where
    return = pure
    PRight b >>= _  = PRight b
    PLeft  a >>= f  = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
    arbitrary = frequency [(1, PLeft <$> arbitrary), (1, PRight <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
    (=-=) (PLeft a) (PLeft a') = property (a == a')
    (=-=) (PRight b) (PRight b') = property (b == b')
    (=-=) _ _ = property False

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure  = Identity
    (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
    return = pure
    (Identity a) >>= f  = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = frequency [(1, Identity <$> arbitrary)]

instance (Eq a) => EqProp (Identity a) where
    (=-=) (Identity a) (Identity a') = property (a == a')

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
    (<>) Nil xs = xs
    (<>) xs Nil = xs
    (<>) (Cons x xs) ys = Cons x (xs <> ys)

instance Monoid (List a) where
    mempty = Nil
    mappend = (<>)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
    pure a = Cons a (Nil)
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> Cons x xs = Cons (f x) (f <$> xs) <> (fs <*> Cons x xs)

instance Monad List where
    return = pure
    Nil >>= f =  Nil
    Cons x xs >>= f = (f x) <> (xs >>= f)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = oneof [pure Nil, Cons <$> arbitrary <*> arbitrary]

instance (Eq a) => EqProp (List a) where
    (=-=) Nil Nil = property True
    (=-=) Nil _ = property False
    (=-=) _ Nil = property False
    (=-=) (Cons x xs) (Cons x' xs') = property (x == x') .&&. property (xs == xs')

j :: Monad m => m (m a) -> m a
j xs = xs >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f xs = xs >>= (\x -> return $ f x)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f as bs = (return f) <*> as <*> bs

a :: Monad m => m a -> m (a -> b) -> m b
a as f = f <*> as

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) f = do
    y <- f a
    ys <- meh as f
    return (y : ys)

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs (\x -> x >>= \y -> return y)


main :: IO ()
main = do
    let triggerNope :: Nope (Int, String, Int)
        triggerNope = undefined
        triggerBahEither :: BahEither String (Int, String, Int)
        triggerBahEither = undefined
        triggerIdentity :: Identity (Int, String, Int)
        triggerIdentity = undefined
        triggerList :: List (Int, String, Int)
        triggerList = undefined
    putStrLn "Nope"
    quickBatch $ monad triggerNope
    putStrLn "BahEither"
    quickBatch $ monad triggerBahEither
    putStrLn "Identity"
    quickBatch $ monad triggerIdentity
    putStrLn "List"
    quickBatch $ monad triggerList
