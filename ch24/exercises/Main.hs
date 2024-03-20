module Exercises where

import Control.Applicative
import Text.Trifecta
import Data.Char

data NumberOrString = NOSS String | NOSI Integer deriving (Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show)

instance Eq NumberOrString where
    (==) (NOSS s) (NOSS s') = s == s'
    (==) (NOSI i) (NOSI i') = i == i'
    (==) _ _ = False

instance Ord NumberOrString where
    compare (NOSS s) (NOSS s') = compare s s'
    compare (NOSI i) (NOSI i') = compare i i'
    compare _ _ = undefined

instance Eq SemVer where
    (==) (SemVer major minor patch release metadata) (SemVer major' minor' patch' release' metadata') = (major, minor, patch, release, metadata) == (major', minor', patch', release', metadata')

instance Ord SemVer where
    compare (SemVer major minor patch release metadata) (SemVer major' minor' patch' release' metadata') = case compare major major' of
                                                                                GT -> GT
                                                                                LT -> LT
                                                                                EQ -> case compare minor minor' of
                                                                                        GT -> GT
                                                                                        LT -> LT
                                                                                        EQ -> case compare patch patch' of
                                                                                            GT -> GT
                                                                                            LT -> LT
                                                                                            EQ -> compare release release'
parseAsAWhole :: Parser Integer
parseAsAWhole = do
    n <- integer
    eof

    return n

parseNOSS :: Parser String
parseNOSS = do
    x <- alphaNum
    xs <- many alphaNum

    return $ x : xs

parseNOSI :: Parser Integer
parseNOSI = do
    i <- integer

    return i

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = (NOSS <$> try parseNOSS) <|> (NOSI <$> parseNOSI)

parseIdent :: Parser NumberOrString
parseIdent = do
    ident <- parseNumberOrString

    return ident

parseNextIdent :: Parser NumberOrString
parseNextIdent = do
    char '.'
    ident <- parseNumberOrString

    return ident

parseRelease :: Parser [NumberOrString]
parseRelease = do
    char '-'
    x <- parseIdent
    xs <- many parseNextIdent

    return $ x : xs

parseMetadata :: Parser [NumberOrString]
parseMetadata = do
    char '+'
    x <- parseIdent
    xs <- many parseNextIdent

    return $ x : xs

parseSemVer :: Parser SemVer
parseSemVer = do
    major <- integer
    char '.'
    minor <- integer
    char '.'
    patch <- integer
    release <- option [] parseRelease
    metadata <- option [] parseMetadata
    eof

    return $ SemVer major minor patch release metadata

parseNonDigit :: Parser Char
parseNonDigit = do
    c <- characterChar
    case c of
        '0' -> unexpected "unexpected digit"
        '1' -> unexpected "unexpected digit"
        '2' -> unexpected "unexpected digit"
        '3' -> unexpected "unexpected digit"
        '4' -> unexpected "unexpected digit"
        '5' -> unexpected "unexpected digit"
        '6' -> unexpected "unexpected digit"
        '7' -> unexpected "unexpected digit"
        '8' -> unexpected "unexpected digit"
        '9' -> unexpected "unexpected digit"
        _   -> return c

parseDigit :: Parser Char
parseDigit = do
    c <- characterChar

    case c of
        '0' -> return '0'
        '1' -> return '1'
        '2' -> return '2'
        '3' -> return '3'
        '4' -> return '4'
        '5' -> return '5'
        '6' -> return '6'
        '7' -> return '7'
        '8' -> return '8'
        '9' -> return '9'
        _   -> unexpected "digit expected"

base10Integer :: Parser Integer
base10Integer = do
    m  <- option '+' $ char '-'
    x  <- parseDigit
    xs <- manyTill parseDigit (try parseNonDigit)

    return $ go m 0 (x : xs) where go :: Char -> Integer -> [Char] -> Integer
                                   go mult n [] = case mult of
                                                    '+' -> n
                                                    '-' -> (-1) * n
                                   go mult n (x : xs) = go mult (n * 10 + toInteger (ord x - ord '0')) xs


type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

makeInt :: Char -> Char -> Char -> Char -> Int
makeInt th h te o = 1000 * digitToInt (th) + 100 * digitToInt (h) + 10 * digitToInt (te) + digitToInt (o)

parsePhoneRaw :: Parser PhoneNumber
parsePhoneRaw = do
    npa_h <- digit
    npa_t <- digit
    npa_o <- digit

    e_h <- digit
    e_t <- digit
    e_o <- digit

    ln_th <- digit
    ln_h <- digit
    ln_te <- digit
    ln_o <- digit

    return $ PhoneNumber (makeInt '0' npa_h npa_t npa_o) (makeInt '0' e_h e_t e_o) (makeInt ln_th ln_h ln_te ln_o)

parsePhoneDash :: Parser PhoneNumber
parsePhoneDash = do
    npa_h <- digit
    npa_t <- digit
    npa_o <- digit

    char '-'

    e_h <- digit
    e_t <- digit
    e_o <- digit

    char '-'

    ln_th <- digit
    ln_h <- digit
    ln_te <- digit
    ln_o <- digit

    return $ PhoneNumber (makeInt '0' npa_h npa_t npa_o) (makeInt '0' e_h e_t e_o) (makeInt ln_th ln_h ln_te ln_o)

parsePhoneBracket :: Parser PhoneNumber
parsePhoneBracket = do
    char '('

    npa_h <- digit
    npa_t <- digit
    npa_o <- digit

    char ')'
    space

    e_h <- digit
    e_t <- digit
    e_o <- digit

    char '-'

    ln_th <- digit
    ln_h <- digit
    ln_te <- digit
    ln_o <- digit

    return $ PhoneNumber (makeInt '0' npa_h npa_t npa_o) (makeInt '0' e_h e_t e_o) (makeInt ln_th ln_h ln_te ln_o)

parsePhoneFull :: Parser PhoneNumber
parsePhoneFull = do
    char '1'
    char '-'

    npa_h <- digit
    npa_t <- digit
    npa_o <- digit

    char '-'

    e_h <- digit
    e_t <- digit
    e_o <- digit

    char '-'

    ln_th <- digit
    ln_h <- digit
    ln_te <- digit
    ln_o <- digit

    return $ PhoneNumber (makeInt '0' npa_h npa_t npa_o) (makeInt '0' e_h e_t e_o) (makeInt ln_th ln_h ln_te ln_o)

parsePhone :: Parser PhoneNumber
parsePhone = try parsePhoneDash <|> try parsePhoneBracket <|> try parsePhoneFull <|> parsePhoneRaw

main :: IO ()
main = do
    print $ parseString parseNumberOrString mempty "0123"
    print $ parseString parseNumberOrString mempty "12alpha"
    print $ parseString parseSemVer mempty "2.1.1"
    print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
    print $ parseString parseSemVer mempty "1.0.0-gamma+002"
    print $ parseString parseSemVer mempty "1.0.0-beta+oof.sha.41af286"
    print $ parseString parseDigit mempty "123"
    print $ parseString parseDigit mempty "abc"
    print $ parseString base10Integer mempty "123abc"
    print $ parseString base10Integer mempty "abc"
    print $ parseString base10Integer mempty "-123abc"
    print $ parseString parsePhone mempty "1234567890"
    print $ parseString parsePhone mempty "123-456-7890"
    print $ parseString parsePhone mempty "(123) 456-7890"
    print $ parseString parsePhone mempty "1-123-456-7890"
