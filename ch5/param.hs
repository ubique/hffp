-- param.hs

module Parametricity where

myid :: a -> a
myid a = id a

myfunc :: a -> a -> a
myfunc a = id

myfunc2 :: a -> a -> a
myfunc2 a b = a

myfunc3 :: a -> a -> a
myfunc3 a b = b
