-- Scans.hs

module Scans where

fibs = 1 : scanl (+) 1 fibs
fibs' = take 20 $ 1 : scanl (+) 1 fibs
fibs'' = takeWhile (\x -> x < 100) $ 1 : scanl (+) 1 fibs
-- fibs = 1 : scanl (+) 1 fibs
-- fibs = 1 : scanl (+) 1 [1 : scanl (+) 1 fibs]
-- fibs = 1 : scanl (+) 1 [1 : scanl (+) 1 [1 : scanl (+) 1 fibs]]
-- fibs = 1 : scanl (+) 1 [1 : scanl (+) 1 [1 : scanl (+) 1 [1 : scanl (+) 1 fibs]]]

factorial n = scanl (*) 1 [1..] !! n
