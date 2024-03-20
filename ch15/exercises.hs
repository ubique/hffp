-- exercises.hs

module Exercises where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    Nada   <> b       = b
    a      <> Nada    = a
    Only a <> Only b  = Only (a <> b)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend = (<>)

type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj = e <> "! he said " <>
                            adv <> " as he jumped into his car " <>
                            noun <> " and drove off with his " <>
                            adj <> " wife."


madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj = mconcat [e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife."]

asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c = a <> (b <> c) == (a <> b) <> c

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type S = String
type B = Bool
type MA = S -> S -> S -> B

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m -> Bool
monoidLeftIdentity m = (mempty <> m) == m

monoidRightIdentity :: (Eq m, Monoid m)
                   => m -> Bool
monoidRightIdentity m = (m <> mempty) == m

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools)
                          , (1, return Twoo) ]

instance Semigroup Bull where
    (<>) _ _ = Fools

instance Monoid Bull where
    mempty = Fools

newtype First' a =
        First' { getfirst' :: Optional a }
        deriving (Eq, Show)

firstGen :: (Arbitrary a) => Gen (First' a)
firstGen = do
    a <- arbitrary
    oneof [return $ First' Nada,  return $ First' (Only a)]

instance Arbitrary a => Arbitrary (First' a)  where
    arbitrary = firstGen

instance Semigroup (First' a) where
    (<>) (First' (Nada)) x = x
    (<>) x (First' (Nada)) = x
    (<>) x y = x

instance Monoid (First' a) where
    mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
               (a <> (b <> c)) == ((a <> b) <> c)

newtype Identity a = Identity a deriving (Show)
instance (Eq a) => Eq (Identity a) where
    (Identity x) == (Identity y) = x == y
instance (Semigroup a) => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)
identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    a <- arbitrary
    return (Identity a)
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

data Two a b = Two a b deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')
twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = twoGen
instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

data Three a b c = Three a b c deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')
threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = threeGen

data Four a b c d = Four a b c d deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four w x y z) <> (Four w' x' y' z') = Four (w <> w') (x <> x') (y <> y') (z <> z')
fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = fourGen

newtype BoolConj = BoolConj Bool deriving (Eq, Show)
instance Semigroup (BoolConj) where
    (BoolConj x) <> (BoolConj y) = BoolConj (x && y)
instance Arbitrary BoolConj where
    arbitrary = oneof [return $ BoolConj False, return $ BoolConj True]
instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
instance Semigroup (BoolDisj) where
    (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)
instance Arbitrary BoolDisj where
    arbitrary = oneof [return $ BoolDisj False, return $ BoolDisj True]
instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

data Or a b = Fst a | Snd b deriving (Eq, Show)
instance Semigroup (Or a b) where
    (Snd x) <> _ = Snd x
    _ <> (Snd x) = Snd x
    (Fst x) <> (Fst x') = Fst x'
orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Fst a, return $ Snd b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = orGen

newtype Combine a b = Combine { unCombine :: (a -> b) }
instance (Semigroup b) => Semigroup (Combine a b) where
    Combine f <> Combine f' = Combine (\n -> f (n) <> f' (n))

instance (Monoid b) => Monoid (Combine a b) where
    mempty = Combine (\n -> mempty)
    mappend = (<>)

instance (Show a) => Show (Combine a b) where
    show (Combine f) = "Combine instance"

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    coarbitrary [] = ZipList' []
    coarbitrary (x:xs) = ZipList' [x:xs]

combineAssoc :: (Eq b, Semigroup b) => Combine a b -> Combine a b -> Combine a b -> a -> Bool
combineAssoc w x y z  = (unCombine ((w <> x) <> y) $ z) == (unCombine (w <> (x <> y)) $ z)

combineLeftIdentity ::  (Eq b, Monoid b) => Combine a b -> a -> Bool
combineLeftIdentity m a = (mempty <> (unCombine m $ a)) == (unCombine m $ a)

combineRightIdentity ::  (Eq b, Monoid b) => Combine a b -> a -> Bool
combineRightIdentity m a = ((unCombine m $ a) <> mempty) == (unCombine m $ a)

newtype Comp a = Comp { unComp :: (a -> a) }
instance (Semigroup a) => Semigroup (Comp a) where
    Comp f <> Comp f' = Comp (\n -> f (n) <> f' (n))
instance (Monoid a) => Monoid (Comp a) where
    mempty = Comp (\n -> mempty)
    mappend = (<>)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    arbitrary = do
        f <- arbitrary
        return $ Comp f

compAssoc :: (Eq a, Semigroup a) => Comp a -> Comp a -> Comp a -> a -> Bool
compAssoc w x y z  = (unComp ((w <> x) <> y) $ z) == (unComp (w <> (x <> y)) $ z)
instance Show (Comp f) where
    show (Comp f) = "Comp instance"

compLeftIdentity :: (Eq a, Monoid a) => Comp a -> a -> Bool
compLeftIdentity m a = (mempty <> (unComp m $ a)) == (unComp m $ a)

compRightIdentity :: (Eq a, Monoid a) => Comp a -> a -> Bool
compRightIdentity m a = ((unComp m $ a) <> mempty) == (unComp m $ a)

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity (mempty)
    mappend = (<>)

newtype Mem s a = Mem {
        runMem :: s -> (a, s)
    }

instance (Semigroup a) => Semigroup (Mem s a) where
    (Mem f) <> (Mem f') = Mem (\s -> (fst (f s) <> fst (f' s), snd (f $ snd (f' s))))

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem (\s -> (mempty, s))

instance (CoArbitrary s, Arbitrary a, Arbitrary s) => Arbitrary (Mem s a) where
    arbitrary = do
        f <- arbitrary
        return $ Mem f

instance Show (Mem s a) where
    show (Mem f) = "Mem s ainstance"

memLeftIdentity :: (Eq a, Eq s, Monoid a, Monoid s) => Mem s a -> s -> Bool
memLeftIdentity m s = (mempty <> (runMem m $ s)) == (runMem m $ s)

memRightIdentity :: (Eq a, Eq s, Monoid a, Monoid s) => Mem s a -> s -> Bool
memRightIdentity m s = ((runMem m $ s) <> mempty) == (runMem m $ s)

f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
    quickCheck (monoidAssoc :: Bull -> Bull -> Bull -> Bool)
    quickCheck (monoidLeftIdentity :: Bull -> Bool)
    quickCheck (monoidRightIdentity :: Bull -> Bool)
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
    quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
    quickCheck (semigroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)
    quickCheck (semigroupAssoc :: Two [Int] String -> Two [Int] String -> Two [Int] String -> Bool)
    quickCheck (semigroupAssoc :: Three [Int] String [String] -> Three [Int] String [String] -> Three [Int] String [String] -> Bool)
    quickCheck (semigroupAssoc :: Four [Int] String [String] String -> Four [Int] String [String] String -> Four [Int] String [String] String -> Bool)
    quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
    quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
    quickCheck (semigroupAssoc :: Or Int String -> Or Int String -> Or Int String -> Bool)
    quickCheck (combineAssoc   :: Combine Int String -> Combine Int String -> Combine Int String -> Int -> Bool)
    quickCheck (compAssoc :: Comp String -> Comp String -> Comp String -> String -> Bool)
    quickCheck (monoidAssoc :: Trivial -> Trivial -> Trivial -> Bool)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)
    quickCheck (monoidAssoc :: Identity String -> Identity String -> Identity String -> Bool)
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)
    quickCheck (monoidAssoc :: Two [Int] String -> Two [Int] String -> Two [Int] String -> Bool)
    quickCheck (monoidLeftIdentity :: Two [Int] String -> Bool)
    quickCheck (monoidRightIdentity :: Two [Int] String -> Bool)
    quickCheck (monoidAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)
    quickCheck (monoidAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
    quickCheck (monoidAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
    quickCheck (combineLeftIdentity :: Combine [Int] String -> [Int] -> Bool)
    quickCheck (combineRightIdentity :: Combine [Int] String -> [Int] -> Bool)
    quickCheck (compLeftIdentity :: Comp String -> String -> Bool)
    quickCheck (compRightIdentity :: Comp String -> String -> Bool)
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0
    quickCheck (memLeftIdentity :: Mem [Int] String -> [Int] -> Bool)
    quickCheck (memRightIdentity :: Mem [Int] String -> [Int] -> Bool)
