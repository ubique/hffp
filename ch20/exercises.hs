-- Exercises.hs

import Data.Monoid
import Data.Maybe

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = any (== x) xs

elemHelper :: Eq a => a -> a -> Bool -> Bool
elemHelper needle x False = (needle == x)
elemHelper _ _ True = True

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' needle xs = foldr (\x -> \y -> elemHelper needle x y) False xs

minimumHelper :: Ord a => a -> Maybe a -> Maybe a
minimumHelper x Nothing = Just x
minimumHelper x (Just y) = Just (min x y)

minimum' :: (Foldable t, Ord a) => t a -> a
minimum' = fromJust . foldr minimumHelper Nothing

maximumHelper :: Ord a => a -> Maybe a -> Maybe a
maximumHelper x Nothing = Just x
maximumHelper x (Just y) = Just (max x y)

maximum' :: (Foldable t, Ord a) => t a -> a
maximum' = fromJust . foldr maximumHelper Nothing

null' :: (Foldable t) => t a -> Bool
null' = isNothing . foldr (\x -> \y -> Just x) Nothing

length' :: (Foldable t) => t a -> Int
length' = foldr (\x -> \l -> l + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\x -> \l -> x : l) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr (\x -> \y -> mappend (f x) y) mempty xs

data Constant a b = Constant b
instance Foldable (Constant a) where
    foldMap f (Constant b) = f b

data Constant' a b = Constant' b
instance Foldable (Constant' a) where
    foldr f a _ = a

data Two a b = Two a b
instance Foldable (Two a) where
    foldMap f (Two _ b) = f b

data Two' a b = Two' a b
instance Foldable (Two' a) where
    foldr f x (Two' _ b) = f b x

data Three a b c = Three a b c
instance Foldable (Three a b) where
    foldr f z (Three _ _ c) =  f c z

data Three' a b c = Three' a b c
instance Foldable (Three' a b) where
    foldMap f (Three' _ _ c) = f c

data Three'' a b = Three'' a b b
instance Foldable (Three'' a) where
    foldMap f (Three'' _ b b') = f b <> f b'

data Three''' a b = Three''' a b b
instance Foldable (Three''' a) where
    foldr f z (Three''' _ b b') = f b' (f b z)

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
    foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''

data Four'' a b = Four'' a b b b
instance Foldable (Four'' a) where
    foldr f z (Four'' _ b b' b'') = f b'' (f b' (f b z))

filterFHelper :: (a -> Bool) -> a -> Maybe a
filterFHelper f a = case f a of
                        True -> Just a
                        False -> Nothing

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
