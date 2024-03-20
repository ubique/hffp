-- Vigenere.hs

module Vigenere where

import Control.Monad (forever)
import Data.Char
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)

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

runCipher :: IO ()
runCipher = do
    putStr "Plaintext: "
    plaintext <- getLine
    putStr "Passphrase: "
    passphrase <- getLine
    putStrLn $ "Cipher: " ++ vigenere plaintext passphrase


runDecipher :: IO ()
runDecipher = do
    putStr "Ciphertext: "
    ciphertext <- getLine
    putStr "Passphrase: "
    passphrase <- getLine
    putStrLn $ "Cipher: " ++ unVigenere ciphertext passphrase

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    forever $ do
        putStrLn "Do you want to 1) cipher or 2) decipher?"
        action <- getLine
        case action of
            "1" -> runCipher
            "2" -> runDecipher
            _ ->  putStrLn "Emm. What?"
