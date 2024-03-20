module Main where

{-# LANGUAGE FlexibleContexts #-}

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

newtype Identity' a = Identity' a deriving (Eq, Ord, Show)

instance Functor Identity' where
    fmap f (Identity' a) = Identity' (f a)

instance Applicative Identity' where
    pure = Identity'
    (Identity' f) <*> (Identity' v) = Identity' (f v)

instance Foldable Identity' where
    foldMap f (Identity' a) = f a

instance Traversable Identity' where
    traverse f (Identity' a) = Identity' <$> (f a)

instance (Arbitrary a) => Arbitrary (Identity' a) where
    arbitrary = Identity' <$> arbitrary

instance (Eq a) => EqProp (Identity' a) where
    (=-=) (Identity' x) (Identity' y) = property (x == y)


newtype Constant' a b = Constant' { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant' a) where
    fmap _ (Constant' a) = Constant' a

instance (Monoid a) => Applicative (Constant' a) where
    pure _ = Constant' mempty
    (Constant' x) <*> (Constant' y) = Constant' (mappend x y)

instance Foldable (Constant' a) where
    foldMap _ (Constant' a) = mempty

instance Traversable (Constant' a) where
    traverse f (Constant' a) = pure $ Constant' a

instance (Arbitrary a) => Arbitrary (Constant' a b) where
    arbitrary = Constant' <$> arbitrary

instance (Eq a) => EqProp (Constant' a b) where
    (=-=) (Constant' x) (Constant' y) = property (x == y)


data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor (Optional) where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep $ f a

instance Applicative (Optional) where
    pure = Yep
    (Nada) <*> _ = Nada
    _ <*> Nada = Nada
    Yep f <*> Yep v = Yep $ f v

instance Foldable (Optional) where
    foldMap _ Nada = mempty
    foldMap f (Yep v) = f v

instance Traversable (Optional) where
    traverse f (Nada) = pure Nada
    traverse f (Yep v) = Yep <$> (f v)

instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary = frequency [(1, return Nada), (1, Yep <$> arbitrary)]

instance (Eq a) => EqProp (Optional a) where
    (=-=) Nada Nada = property True
    (=-=) Nada _ = property False
    (=-=) _ Nada = property False
    (=-=) (Yep x) (Yep y) = property (x == y)


data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => (Applicative (Three a b)) where
    pure = Three mempty mempty
    (Three a b c) <*> (Three a' b' c') = Three (mappend a a') (mappend b b') (c c')

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> (arbitrary)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c ) where
    (=-=) (Three a b c) (Three a' b' c') = property (a == a' && b == b' && c == c')


data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
    fmap f (Pair a b)  = Pair a (f b)

instance (Monoid a) => (Applicative (Pair a)) where
    pure = Pair mempty
    (Pair a b) <*> (Pair a' b') = Pair (mappend a a') (b b')

instance Foldable (Pair a) where
    foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
    traverse f (Pair a b) = Pair a <$> (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
    (=-=) (Pair a b) (Pair a' b') = property (a == a' && b == b')


data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
    fmap f (Big a b b')  = Big a (f b) (f b')

instance (Monoid a) => (Applicative (Big a)) where
    pure x = Big mempty x x
    (Big a b b') <*> (Big a' b'' b''') = Big (mappend a a') (b b'') (b' b''')

instance Foldable (Big a) where
    foldMap f (Big a b b') = mappend (f b) (f b')

instance Traversable (Big a) where
    traverse f (Big a b b') = Big a <$> (f b) <*> (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
    (=-=) (Big a b b') (Big a' b'' b''') = property (a == a' && b == b'' && b' == b''')


data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
    fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance (Monoid a) => (Applicative (Bigger a)) where
    pure x = Bigger mempty x x x
    (Bigger a b b' b'') <*> (Bigger a' b''' b'''' b''''') = Bigger (mappend a a') (b b''') (b' b'''') (b'' b''''')

instance Foldable (Bigger a) where
    foldMap f (Bigger a b b' b'') = (f b) <> (f b') <> (f b'')

instance Traversable (Bigger a) where
    traverse f (Bigger a b b' b'') = Bigger a <$> (f b) <*> (f b') <*> (f b'')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
    arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
    (=-=) (Bigger a b b' b'') (Bigger a' b''' b'''' b''''') = property (a == a' && b == b''' && b' == b'''' && b'' == b''''')


data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap f (S xs x) = S (fmap f xs) (f x)

instance (Applicative n) => Applicative (S n) where
    pure x = S (pure x) x
    (S fs f) <*> (S xs x) = S (fs <*> xs) (f x)

instance (Foldable n) => Foldable (S n) where
    foldMap f (S xs x) = mappend (foldMap f xs) (f x)

instance (Traversable n) => Traversable (S n) where
    traverse f (S xs x) = S <$> (traverse f xs) <*> (f x)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a) where
    (=-=) = eq

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node left a right) = mappend (mappend (foldMap f left) (f a)) (foldMap f right)

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node left a right) = Node <$> (traverse f left) <*> (f a) <*> (traverse f right)

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = frequency[(1, return Empty), (1, Leaf <$> arbitrary), (1, Node <$> arbitrary <*> arbitrary <*> arbitrary)]


instance (Eq a, EqProp a) => EqProp (Tree a) where
    (=-=) = eq

main :: IO ()
main = do
    let trigger_identity :: Identity' (Maybe Int, String, Char, [Int])
        trigger_identity = undefined
    let trigger_constant :: Constant' String (Maybe Int, String, Char, [Int])
        trigger_constant = undefined
    let trigger_optional :: Optional (Maybe Int, String, Char, [Int])
        trigger_optional = undefined
    let trigger_three :: Three String String (Maybe Int, String, Char, [Int])
        trigger_three = undefined
    let trigger_pair :: Pair String (Maybe Int, String, Char, [Int])
        trigger_pair = undefined
    let trigger_big :: Big String (Maybe Int, String, Char, [Int])
        trigger_big = undefined
    let trigger_bigger :: Bigger String (Maybe Int, String, Char, [Int])
        trigger_bigger = undefined
    let trigger_sna :: S [] (Maybe Int, String, Char, [Int])
        trigger_sna = undefined
    let trigger_tree :: Tree (Maybe Int, String, Char, [Int])
        trigger_tree = undefined
    quickBatch (traversable trigger_identity)
    quickBatch (traversable trigger_constant)
    quickBatch (traversable trigger_optional)
    quickBatch (traversable trigger_three)
    quickBatch (traversable trigger_pair)
    quickBatch (traversable trigger_big)
    quickBatch (traversable trigger_bigger)
    quickBatch (traversable trigger_sna)
    quickBatch (traversable trigger_tree)
