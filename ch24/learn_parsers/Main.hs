module LearnParsers where

import Control.Applicative
import Text.Parser.Combinators (eof, choice)
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = char '1' >> stop

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

oneTwoThreePrefix = (string "123" <|> string "12" <|> string "1") >> eof
oneTwoThreePrefix' = ((char '1' >> char '2' >> char '3') <|> (char '1' >> char '2') <|> char '1') >> eof

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
    pNL "stop:"
    testParse stop

    pNL "one:"
    testParse one

    pNL "one':"
    testParse one'

    pNL "oneTwo:"
    testParse oneTwo

    pNL "oneTwo':"
    testParse oneTwo'

test1 = do
    pNL "one >> eof"
    print $ parseString (one >> eof) mempty "1"

    pNL "one >> eof"
    print $ parseString (one >> eof) mempty "12"

    pNL "one >> eof"
    print $ parseString (oneTwo >> eof) mempty "12"

    pNL "one >> eof"
    print $ parseString (oneTwo >> eof) mempty "123"

    pNL "oneTwoThree:"
    print $ parseString oneTwoThreePrefix mempty "1"
    print $ parseString oneTwoThreePrefix mempty "12"
    print $ parseString oneTwoThreePrefix mempty "123"
    print $ parseString oneTwoThreePrefix mempty "1234"
    print $ parseString oneTwoThreePrefix' mempty "123"
