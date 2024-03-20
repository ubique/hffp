-- exercises.hs

import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m

twiceWhenEvent :: [Integer] -> [Integer]
twiceWhenEvent xs = do
    x <- xs
    if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEvent' :: [Integer] -> [Integer]
twiceWhenEvent' xs = do
    x <- xs
    if even x
    then [x*x, x*x]
    else []

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

f':: Maybe Integer
f' = Just 0

g :: Integer -> Maybe Integer
g i =
    if even i
    then Just (i + 1)
    else Nothing

g' :: Maybe Integer
g' = Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

h' :: Maybe Integer
h' = Just 0

doSomething n = do
    a <- f'
    b <- g'
    c <- h'
    pure (a, b, c)

doSomething' n = do
    a <- f n
    b <- g a
    c <- h b
    pure (a, b, c)

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second
    (<*>) (First a) _ = First a
    (<*>) _ (First a) = First a
    (<*>) (Second f) (Second x) = Second (f x)

instance Monad (Sum a) where
    return = pure
    (>>=) (First a) _ = First a
    (>>=) (Second b) f = f b
