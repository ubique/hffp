-- PoemLines.hs

module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

splitBySymbol :: String -> Char -> [String]
splitBySymbol [] s = []
splitBySymbol str s = (takeWhile (/= s) str): (splitBySymbol (drop 1 (dropWhile (/= s) str)) s)

myLines :: String -> [String]
myLines [] = []
myLines x = splitBySymbol x '\n'

shouldEqual = [ "Tyger Tyger, burning bright"
                , "In the forests of the night"
                , "What immortal hand or eye"
                , "Could frame thy fearful symmetry?"
            ]

main :: IO ()
main =
    print $
    "Are they equal? "
    ++ show (myLines sentences == shouldEqual)
