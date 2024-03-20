-- cs.hs

module Cs where

x = (+)

f xs = w `x` 1
    where w = length xs

y :: a -> a
y x = x

z :: (a, b) -> a
z a = fst a
