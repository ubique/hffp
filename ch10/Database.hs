-- Database.hs

import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [
    DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, World!",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = foldl go [] xs where
                    go x (DbDate t) = t : x
                    go x _ = x

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = foldr go [] xs where
                        go (DbNumber n) x  = n : x
                        go _ x = x

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = head . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr go 0 xs where
            go (DbNumber n) acc = n + acc
            go _ acc = acc

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral (sumDb xs) / fromIntegral (length $ filterDbNumber xs)
