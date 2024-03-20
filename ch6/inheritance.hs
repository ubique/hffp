-- Inheritance.hs

module Inheritance where

newtype Nada = Nada Double deriving (Eq, Show)

instance Num Nada where
    (Nada x) + (Nada y) = Nada (x + y)
    (Nada x) * (Nada y) = Nada (x * y)
    abs (Nada x) = if x < 0 then Nada (-x) else Nada (x)
    signum (Nada x) = if x < 0 then -1 else if x == 0 then 0 else 1
    fromInteger x = Nada (fromInteger (x))
    negate (Nada x) = Nada (-x)

instance Fractional Nada where
    (Nada x) / (Nada y) = Nada (x / y)
    recip (Nada n) = Nada (recip n)
    fromRational r = Nada (fromRational r)
