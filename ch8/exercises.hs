-- exercises.hs

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

sumN :: (Eq a, Num a) => a -> a
sumN 0 = 0
sumN n = n + sumN (n - 1)

mul :: (Integral a) => a -> a -> a
mul x 0 = 0
mul x y = x + mul x (y - 1)

data DivideResult = Result Integer | DividedByZero deriving (Show)

dividedBy :: Integral a => a -> a -> DivideResult
dividedBy num denom = go absNum absDenom 0 sign
        where absNum = abs num
              absDenom = abs denom
              sign = toInteger(signum (num) * signum (denom))
              go n d c s
                | d == 0 = DividedByZero
                | n < d = Result (s * c)
                | otherwise = go (n - d) d (c + 1) s

mc91 :: Integer -> Integer
mc91 x
    | x > 100 = x - 10
    | otherwise = mc91 $ mc91 (x + 11)
