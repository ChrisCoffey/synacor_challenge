import Data.List            (permutations)

--red, shiny, blue, corroded, concave
coins :: [Int]
coins = [2, 5, 9, 3, 7]
equation :: Int -> Int -> Int -> Int -> Int -> Int
equation a b c d e = a + b * (c^2) + (d^3) - e

--blue red shiny concave corroded
res = filter(\ls@[a,b,c,d,e]-> 399 == (equation a b c d e)) . permutations $ coins
