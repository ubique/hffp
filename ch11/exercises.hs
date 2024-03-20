-- exercises.hs

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Exercises where

import Data.Char

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar (Plane _ _) = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane (Car _ _) = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu
getManu (Plane _ _) = undefined

data Example = MakeExample deriving Show
data Example' = MakeExample' Int deriving Show

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
    tooMany (Goats n) = n > 43

instance TooMany (Int, String) where
    tooMany (n, s) = n > 44

instance TooMany (Int, Int) where
    tooMany (x, y) = x + y > 44

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany (x + y)


data Person' = MkPerson' String Int deriving (Eq, Show)
namae :: Person' -> String
namae (MkPerson' s _) = s

jm' = MkPerson' "julie" 108
ca' = MkPerson' "chris" 16


data Person = Person { name' :: String
                     , age' :: Int }
                     deriving (Eq, Show)

jm = Person "julie" 108
ca = Person "chris" 16

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show

type AuthorName = String
data Author' = Author' (AuthorName, BookType)
data Author = Fiction' AuthorName
            | Nonfiction' AuthorName
            deriving (Eq, Show)

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden' =
    Garden Gardener FlowerType
    deriving Show

data Garden = Gardenia' Gardener
            | Daisy' Gardener
            | Rose' Gardener
            | Lilac' Gardener
            deriving Show

data GuessWhat = ChickenButt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a
                                       , psecond :: b }
                                       deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWhool = Int
data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWhool deriving (Eq, Show)

data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

trivialValue :: GuessWhat
trivialValue = ChickenButt

idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

data Twitter = Twitter deriving (Eq, Show)
data AskFm = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

data SocialNetwork = Twitter' | AskFm' deriving (Eq, Show)

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct { pfirst = 100, psecond = 100.1 }

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgLang = Haskell
              | Agda
              | Idris
              | PureScript
              deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem, lang :: ProgLang } deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = undefined


newtype Acres = Acres Int deriving Show

data FarmerType  = DairyFarmer | WheatFarmer | SoybeanFarmer deriving (Eq, Show)

data Farmer = Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = FarmerRec { name :: Name
                           , acres :: Acres
                           , farmerType :: FarmerType } deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = farmerType (farmer) == DairyFarmer

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' a (Leaf) = Node Leaf a Leaf
insert' a (Node left b right)
    | a == b = Node left b right
    | a < b = Node (insert' a left) b right
    | b < a = Node left b (insert' a right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f(a)) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf)
                 1
                 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf)
                   2
                   (Node Leaf 5 Leaf)


mapOkay = if mapTree (+1) testTree' == mapExpected
          then print "yup OK!"
          else error "test failed"


preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : ((preorder left) ++ (preorder right))

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ (a : []) ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ (a : [])

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf)
                2
                (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears"

foldTree :: (a -> b -> b)
            -> b
            -> BinaryTree a
            -> b
foldTree f acc tree = foldr f acc (inorder tree)

main :: IO ()
main = do
    testPreorder
    testInorder
    testPostorder

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf needle haystack = findNeedle needle haystack where
    findNeedle [] _ = True
    findNeedle _ [] = False
    findNeedle needle@(x:xs) (y:ys) = case x == y of
                    False -> findNeedle needle ys
                    True  -> findNeedle xs ys

capitilizeWords :: String -> [(String, String)]
capitilizeWords = map (\x -> (x, toUpper (head x) : drop 1 x)) . words

capitilizeWord :: String -> String
capitilizeWord [] = []
capitilizeWord (x:xs) = toUpper (x) : xs

capitilizeParagraph :: String -> String
capitilizeParagraph paragraph = unwords (go (words paragraph) True) where
    go [] _ = []
    go (x:xs) False = x : (go xs ((last x) == '.'))
    go (x:xs) True = capitilizeWord (x) : (go xs ((last x) == '.'))
