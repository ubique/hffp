-- exercises.hs

module Exercises where

notThe :: String -> Maybe String
notThe s
    | s == "the" = Nothing
    | otherwise = Just s

replaceThe :: String -> String
replaceThe s = unwords (map go (words s)) where
    go w = case notThe w of
        (Just s)  -> s
        (Nothing) -> "a"

replaceThe' :: String -> String
replaceThe' s = case words s of
    []     -> ""
    (x:[]) -> case notThe x of
        (Just s) -> s
        (Nothing) -> "a"
    (x:xs) -> case notThe x of
        (Just s) -> s ++ " " ++ (replaceThe' (unwords xs))
        (Nothing) -> "a " ++ (replaceThe' (unwords xs))

isVowel :: Char -> Bool
isVowel c = case c of
            'a' -> True
            'e' -> True
            'i' -> True
            'o' -> True
            'u' -> True
            _ -> False

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s) where
    go [] = 0
    go (x:[]) = 0
    go (x:xs) | x == "the" = if startsFromVowel (head xs) then 1 else 0 + go xs
              | otherwise = go xs
    startsFromVowel [] = False
    startsFromVowel s = isVowel (head s)

countVowels :: String -> Integer
countVowels = foldr (\x -> \y -> y + if isVowel x then 1 else 0) 0

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s = case (2 * cntVowels) <= length (s) of
                True -> Just (Word' s)
                False -> Nothing
                where cntVowels = foldr (\x -> \y -> y + if elem x vowels then 1 else 0) 0 s

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger (x)

integerToNat :: Integer -> Maybe Nat
integerToNat x = case x >= 0 of
                    True  -> Just $ go x
                    False -> Nothing
                    where go v
                            | v > 0 = Succ (go (v - 1))
                            | v == 0 = Zero
                            | otherwise = undefined

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True


isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe b _ Nothing = b
mayybe b f (Just a) = f (a)

fromMaybe :: a -> Maybe a -> a
fromMaybe defaultValue maybeValue = mayybe defaultValue id maybeValue

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = a : []

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = maybeToList x ++ catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = case any isNothing xs of
                True -> Nothing
                False -> Just $ map f xs where
                    f Nothing = undefined
                    f (Just a) = a

lefts' :: [Either a b] -> [a]
lefts' xs = foldr go [] xs where
                go (Left a) xs = a : xs
                go (Right _) xs = xs

rights' :: [Either a b] -> [b]
rights' xs = foldr go [] xs where
                go (Left _) xs = xs
                go (Right b) xs = b : xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f (b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f (a)
either' _ f (Right b) = f (b)

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e = either' (\x -> Nothing) (\x -> Just $ f x) e

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : (myIterate f $ f(a))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f (b) of
                    Nothing -> []
                    Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f (x)))

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f (a) of
                Nothing -> Leaf
                Just (left, b, right) -> Node (unfold f left) b (unfold f right)

treeBuild :: Integer -> BinaryTree Integer
treeBuild = go 0 where
                go :: Integer -> Integer -> BinaryTree Integer
                go level depth = case level == depth of
                                    False -> Node (go (level + 1) depth) level (go (level + 1) depth)
                                    True -> Leaf
