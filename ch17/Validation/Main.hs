module Main where

data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
    fmap f (Failure e) = Failure e
    fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
    pure  = Success
    (<*>) (Failure x) (Failure y) = Failure (mappend x y)
    (<*>) (Failure x) (Success _) = Failure x
    (<*>) (Success _) (Failure x) = Failure x
    (<*>) (Success x) (Success y) = Success (x y)

data Errors = DividedByZero | StackOverflow | MooglesChewedWires deriving (Eq, Show)

main :: IO ()
main = putStrLn "Hello, Haskell!"
