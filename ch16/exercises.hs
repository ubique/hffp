-- exercises.hs

{-# LANGUAGE FlexibleInstances #-}

module Exercises where

import Control.Monad
import Test.QuickCheck
import GHC.Arr

a = fmap (+1) $ read "[1]" :: [Int]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
c = fmap (*2) $ (\x -> x - 2)
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
e = let ioi = read "1" :: Integer
        changed = read ("123" ++ (show ioi)) :: Integer
        in (*3) changed

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity x = fmap id x == x

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

newtype Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
    a  <- arbitrary
    a' <- arbitrary
    return (Pair a a')

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = pairGen

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) =  Two a (f b)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = twoGen

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) =  Three a b (f c)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = threeGen

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' x y z) =  Three' x (f y) (f z)

three'Gen :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
three'Gen = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three' x y z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = three'Gen

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) =  Four a b c (f d)

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = fourGen

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' w x y z) =  Four' w x y (f z)

four'Gen :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
four'Gen = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four' w x y z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = four'Gen

data Trivial = Trivial deriving (Eq, Show)
-- *Exercises> :k Trivial
-- Trivial :: *

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
    fmap _ (LolNope) = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

-- Why is a Functor instance that applies a function only to First,
-- Either’s Left, impossible? We covered this earlier.
-- kind must be * -> *

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

data Wrap f a = Wrap (f a) deriving (Eq, Show)

-- Chapter exercises
-- Determine whether a valid Functor can be written for the datatype provided:
--
-- 1. data Bool = False | True
--  No:
--  Prelude> :k Bool
--  Bool :: *
-- 2. data BoolAndSomethingElse a = False' a | True' a
-- No: because both data constructors require an argument of type a
-- 3. data BoolAndMaybeSomethingElse a = Falsish | Truish a
--  Yes
data BoolAndMaybeSomethingElse a = Falsish | Truish a
instance Functor BoolAndMaybeSomethingElse where
    fmap _ (Falsish) = Falsish
    fmap f (Truish a) = Truish (f a)
-- 4. newtype Mu f = InF { outF :: f (Mu f) }
-- Prelude> :k Mu
-- Mu :: (* -> *) -> *
-- Yes, if the function can be modified?
-- 5. data D = D (Array Word Word) Int Int
-- No
-- Prelude GHC.Arr> :k D
-- D :: *

-- Rearrange the arguments to the type constructor of the datatype
-- so the Functor instance works:

-- 1.
data Sum' b a = First' a | Second' b
instance Functor (Sum' e) where
    fmap f (First' a) = First' (f a)
    fmap f (Second' b) = Second' b

-- 2.
data Company a c b = DeepBlue a c | Something b
instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

-- 3.
data More b a = L a b a | R b a b deriving (Eq, Show)
instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes:
-- 1.
data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

-- 2.
data K a b = K a
instance Functor (K a) where
    fmap _ (K a) = K a

-- 3.
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype K' a b = K' a
instance Functor (Flip K' a) where
    fmap f (Flip (K' a)) = Flip (K' $ f a)

-- 4.
data EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

-- Do you need something extra to make the instance work?
-- 5.
data LiftItOut f a = LiftItOut (f a) deriving (Show)
instance (Functor f) => Functor (LiftItOut f) where
    fmap f (LiftItOut b) = LiftItOut $ fmap f b

-- 6.
data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa a b) = DaWrappa (fmap f a) (fmap f b)

-- 7.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

-- 8.
data Notorious g o a t = Notorious (g o) (g a) (g t)
instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious x y z) = Notorious x y (fmap f z)

-- 9. You'll need to use recursion:
data List a = Nil | Cons a (List a)
instance Functor (List) where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- 10.
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor (GoatLord) where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat $ f x
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- 11.
data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor (TalkToMe) where
    fmap _ Halt = Halt
    fmap f (Print x y) = Print x (f y)
    fmap f (Read f') = Read (\x -> f (f' (x)))

main :: IO ()
main = do
    quickCheck (functorIdentity :: (Identity Int) -> Bool)
    quickCheck (functorCompose (+1) (*2) :: Identity Int -> Bool)
    quickCheck (functorIdentity :: (Pair String) -> Bool)
    quickCheck (functorCompose (+1) (*2) :: Pair Int -> Bool)
    quickCheck (functorIdentity :: (Two Int String) -> Bool)
    quickCheck (functorCompose (+1) (*2) :: Two String Int -> Bool)
    quickCheck (functorIdentity :: (Three Int [Int] String) -> Bool)
    quickCheck (functorCompose (+1) (*2) :: Three String [Int] Int -> Bool)
    quickCheck (functorIdentity :: (Three' Int String) -> Bool)
    quickCheck (functorCompose (+1) (*2) :: Three' [Int] Int -> Bool)
    quickCheck (functorIdentity :: (Four Int [Int] String [String]) -> Bool)
    quickCheck (functorCompose (+1) (*2) :: Four String [String] [Int] Int -> Bool)
    quickCheck (functorIdentity :: (Four' Int String) -> Bool)
    quickCheck (functorCompose (+1) (*2) :: Four' String Int -> Bool)
