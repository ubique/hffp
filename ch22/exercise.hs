-- exercise.hs

{-# LANGUAGE InstanceSigs #-}

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = (pure f) <*> a <*> b

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
    fmap f (Reader r) = Reader $ f . r

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure f = Reader $ \r -> f
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (<*>) (Reader rab) (Reader ra) = Reader $ \r -> (rab r) (ra r)

instance Monad (Reader r) where
    return = pure
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= arb = Reader $ \r -> runReader (arb (ra r)) r

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
    humanName :: HumanName,
    dogName :: DogName,
    address :: Address
        } deriving (Eq, Show)

data Dog = Dog {
    dogsName :: DogName,
    dogsAddress :: Address
        } deriving (Eq, Show)

