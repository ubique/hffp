-- GuardBlocks.hs

module GuardBlocks where

myAbs :: Integer -> Integer
myAbs x
    | x < 0 = (-x)
    | otherwise = x
