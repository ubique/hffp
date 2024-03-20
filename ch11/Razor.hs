-- Razor.hs

module Razor where

import Data.Char

data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y

go :: Integer -> String
go x = chr (ord '0' + fromInteger(mod x 10)) : (if x < 10 then [] else go (div x 10))

printExpr :: Expr -> String
printExpr (Lit x) = foldl (\y -> \z -> z: y) [] (go x)
printExpr (Add x y) = (printExpr x) ++ " + " ++ (printExpr y)
