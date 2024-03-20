-- arith4.hs

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
    print (roundTrip 4 :: Float)
    print (id 4)
