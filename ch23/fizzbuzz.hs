-- fizzbuzz.hs

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n  | n `mod` 15 == 0   = "FizzBuzz"
            | n `mod` 5 == 0    = "Buzz"
            | n `mod` 3 == 0    = "Fizz"
            | otherwise         = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList xs = execState (mapM_ addResult xs) []

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

fizzBuzzFromToHelper :: Integer -> Integer -> [Integer]
fizzBuzzFromToHelper from to | to < from = []
                             | otherwise = to : (fizzBuzzFromToHelper from (to - 1))

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo from to = fizzBuzzList $ fizzBuzzFromToHelper from to

main :: IO ()
main = do
--    mapM_ putStrLn $ reverse $ fizzBuzzList [1..100]
    mapM_ putStrLn $ fizzBuzzFromTo 1 100
