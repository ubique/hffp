-- Cipher.hs

module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar n [] = []
caesar n (x:xs) = go n x : caesar n xs where
                    go shift x = chr (97 + mod (ord x - 97 + shift) 26)
unCaesar :: Int -> String -> String
unCaesar n xs = caesar (-n) xs
