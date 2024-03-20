-- exercises.hs

import Control.Monad
import System.Exit
import Data.Char

maybeAll :: (a -> Bool) -> Maybe a -> Bool
maybeAll = all

eitherAll :: (a -> Bool) -> Either b a -> Bool
eitherAll = all

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case ((concat (words (map toLower (filter isSymbol line1)))) == (reverse (concat (words (map toLower (filter isSymbol line1)))))) of
        True -> putStrLn "It's a palindrome"
        False -> do putStrLn "Nope!"
                    exitSuccess

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
                putStr "Name: "
                name <- getLine
                putStr "Age: "
                age_str <- getLine
                let age = (read age_str :: Integer)
                case mkPerson name age of
                    Left errorReason -> putStrLn $ "Error: " ++ show errorReason
                    Right person -> putStrLn $ "Yay! Successfully got a person: " ++ show person
