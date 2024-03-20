-- list.hs

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
    (<>) Nil xs = xs
    (<>) xs Nil = xs
    (<>) (Cons x xs) ys = Cons x (xs <> ys)

instance Monoid (List a) where
    mempty  = Nil
    mappend = (<>)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure a = Cons a (Nil)
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f fs) (Cons x xs) = Cons (f x) (f <$> xs) <> (fs <*> Cons x xs)

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)

append :: List a -> List a -> List a
append x y = x <> y

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap (id f) as)

