{-# LANGUAGE MagicHash #-}

module Main where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal

    return (numerator % denominator)

numDigits :: Integer -> Integer
numDigits 0 = 0
numDigits n = 1 + numDigits (div n 10)

toFraction :: Integer -> Double
toFraction n = fromIntegral (n) / (10 ^ numDigits (n))

parseDecimal :: Parser Double
parseDecimal = do
    i <- decimal
    char '.'
    r <- decimal

    return (fromIntegral i + toFraction r)

type RationalOrDecimal = Either Rational Double

parseRationalOrDecimal :: Parser RationalOrDecimal
parseRationalOrDecimal = (Left <$> try parseFraction) <|> (Right <$> try parseDecimal)

main :: IO ()
main = do
    print $ parseString parseFraction mempty "1/2"
    print $ parseString parseDecimal mempty "1.2"
    print $ parseString parseRationalOrDecimal mempty "1/2"
    print $ parseString parseRationalOrDecimal mempty "1.2"
    print $ parseString parseRationalOrDecimal mempty "JOPA"
