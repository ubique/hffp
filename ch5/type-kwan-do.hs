-- type-kwan-do

module TypeKwanDo where

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h a = g $ f a

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w (q (x ))

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz (x), yz(y))

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToY yToWZ x = fst ( yToWZ (xToY (x)))
