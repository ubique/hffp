-- Vigenere.hs

module Vigenere where

import Data.Char

vigenereShift passphrase n = ord (passphrase !! (mod n (length (passphrase)))) - 65
vigenereSubst x shift = chr (64 + mod (ord x - 64 + shift) 26)

vigenere :: String -> String -> String
vigenere [] passphrase = []
vigenere plaintext passphrase = enc 0 plaintext passphrase where
    enc n [] passphrase = []
    enc n (x:xs) passphrase = case x of
                            ' ' -> ' ' : enc n xs passphrase
                            _   -> (encSymbol n x passphrase) : enc (n + 1) xs passphrase
    encSymbol n x passphrase = vigenereSubst x (shift) where
                               shift = vigenereShift passphrase n

unVigenere :: String -> String -> String
unVigenere [] passphrase = []
unVigenere cipher passphrase = unenc 0 cipher passphrase where
    unenc n [] passphrase = []
    unenc n (x:xs) passphrase = case x of
                            ' ' -> ' ' : unenc n xs passphrase
                            _   -> (unencSymbol n x passphrase) : unenc (n + 1) xs passphrase
    unencSymbol n x passphrase = vigenereSubst x (-shift) where
                                 shift = vigenereShift passphrase n
