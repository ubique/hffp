-- validation.hs

module Validation where

import Control.Monad
import Data.Monoid

data Validation a b = Failure a | Success b deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
    (Failure f) <> (Failure f') = Failure (f <> f')
    (Success s) <> (_) = Success s
    (Failure f) <> (Success s) = Success s

main :: IO ()
main = do
    let failure :: String -> Validation String Int
        failure = Failure
        success :: Int -> Validation String Int
        success = Success
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2
