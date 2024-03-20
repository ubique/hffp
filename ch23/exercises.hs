-- exercises.hs

{-# LANGUAGE InstanceSigs #-}

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f m = Moi $ (\s -> let (x, y) = (runMoi m) s in (f (x), y))

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (<*>) (Moi f) (Moi v)  = undefined

instance Monad (Moi s) where
    return = pure
    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    m >>= g = Moi $ \s -> let (x, y) = runMoi m s in runMoi (g x) y

get' :: Moi s s
get' = Moi $ \s -> (s, s)

put' :: s -> Moi s ()
put' s = Moi $ \f -> ((), s)

exec :: Moi s a -> s -> s
exec state s = snd $ runMoi state s

eval :: Moi s a -> s -> a
eval state s = fst $ runMoi state s

modify'' :: (s -> s) -> Moi s ()
modify'' f = Moi $ \s -> ((), f s)
