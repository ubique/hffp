-- Phone.hs

module Phone where

import Data.Char

type Keyboard = [(Char, String)]
data DaPhone = DaPhone Keyboard

keyboard :: Keyboard
keyboard = [
    ('1', ""),
    ('2', "abc"),
    ('3', "def"),
    ('4', "ghi"),
    ('5', "jkl"),
    ('6', "mno"),
    ('7', "pqrs"),
    ('8', "tuv"),
    ('9', "wxyz"),
    ('*', "^"),
    ('0', " +_"),
    ('#', ".,")
    ]

daPhone :: DaPhone
daPhone = DaPhone keyboard

convo :: [String]
convo = [
    "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
    ]

type Digit = Char
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone keyboard) c = canonize c ++ go keyboard (toLower (c)) where
    canonize c = case toLower (c) == c of
                    True  -> []
                    False -> [('*', 1)]
    go [] c = undefined
    go ((k, v):xs) c = case k == c of
                        True -> [(k, 1 + length (v))]
                        False -> go2 v 1 xs c where
                            go2 [] n xs c = go xs c
                            go2 (v:vs) n xs c = case v == c of
                                True -> [(k, n)]
                                False -> go2 vs (n + 1) xs c

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone s = foldl (\x -> \y -> x ++ reverseTaps phone y) [] s

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps xs = foldl (\x -> \y -> x + (snd y)) 0 xs

groupByLetter [] index = index
groupByLetter (x:xs) index = groupByLetter xs (go x index) where
    go x [] = [x]
    go x (i:is) = case fst x == fst i of
                                True -> (fst i, snd x + snd i) : is
                                False ->  i : go x is
mostPopularLetter :: String -> Char
mostPopularLetter s = fst (foldr (\x -> \y -> if snd x < snd y then y else x) ('\0', 0) (groupByLetter (cellPhonesDead daPhone s) []))

coolestLtr :: [String] -> Char
coolestLtr [] = undefined
coolestLtr xs = fst (foldr (\x -> \y -> if snd x < snd y then y else x) ('\0', 0) (go xs [])) where
                    go [] index = index
                    go (x:xs) index = groupByLetter (map (\y -> (y, 1)) x) (go xs index)

coolestWord :: [String] -> String
coolestWord [] = undefined
coolestWord xs = fst (foldr (\x -> \y -> if snd x < snd y then y else x) ("", 0) (go xs [])) where
                    go [] index = index
                    go (x:xs) index = groupByLetter (map (\y -> (y, 1)) (words x)) (go xs index)
