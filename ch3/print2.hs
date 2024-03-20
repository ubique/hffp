-- print2.hs
module Print2 where

main :: IO ()
main = do
    putStrLn "Count four for me"
    putStr   "one, two"
    putStr   ", three, and"
    putStrLn " four!"

