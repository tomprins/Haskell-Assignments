import Data.List
import Data.Unique

--opdracht 1a
differentieer::(Double->Double)->Double->Double->Double
differentieer f p x = (yb-ya)/(xb-xa)
    where yb = f (x+p) 
          ya = f x 
          xb = x+p
          xa = x

--opdracht 1b
integreer::(Double->Double)->Double->Double->Double->Double
integreer f a b p
   |a<b = (f a) * p + integreer f (a+p) b p
   |otherwise = 0 

--opdracht 2
unique::[Int]->[Int]
unique l = nub l

--opdracht 3
poker :: [(Int, Int, Int, Int, Int)]
poker = [(a,b,c,d,e) | a <- [1 .. 6], b <- [1 .. 6], c <- [1 .. 6], d <- [1 .. 6], e <- [1 .. 6], length (group(sort[a,b,c,d,e])) == 1 ]

fourKind :: [(Int, Int, Int, Int, Int)]
fourKind = [(a,b,c,d,e) | a <- [1 .. 6], b <- [1 .. 6], c <- [1 .. 6], d <- [1 .. 6], e <- [1 .. 6], length (group(sort[a,b,c,d,e])) == 2, length (group(sort[a,b,c,d,e]) !! 0 ) == 4 || length (group(sort[a,b,c,d,e]) !! 1 ) == 4 ]

threeKind :: [(Int, Int, Int, Int, Int)]
threeKind = [(a,b,c,d,e) | a <- [1 .. 6], b <- [1 .. 6], c <- [1 .. 6], d <- [1 .. 6], e <- [1 .. 6], length (group(sort[a,b,c,d,e])) == 3, length (group(sort[a,b,c,d,e]) !! 0 ) == 3 || length (group(sort[a,b,c,d,e]) !! 1 ) == 3  || length (group (sort [a,b,c,d,e]) !! 2) == 3]

pair :: [(Int, Int, Int, Int, Int)]
pair = [(a,b,c,d,e) | a <- [1 .. 6], b <- [1 .. 6], c <- [1 .. 6], d <- [1 .. 6], e <- [1 .. 6], length (group(sort[a,b,c,d,e])) == 4]

twoPair :: [(Int, Int, Int, Int, Int)]
twoPair = [(a,b,c,d,e) | a <- [1 .. 6], b <- [1 .. 6], c <- [1 .. 6], d <- [1 .. 6], e <- [1 .. 6], length (group(sort[a,b,c,d,e])) == 3, length (group(sort[a,b,c,d,e]) !! 0 ) /= 3 && length (group(sort[a,b,c,d,e]) !! 1 ) /= 3  && length (group (sort [a,b,c,d,e]) !! 2) /= 3]

fullHouse :: [(Int, Int, Int, Int, Int)]
fullHouse = [(a,b,c,d,e) | a <- [1 .. 6], b <- [1 .. 6], c <- [1 .. 6], d <- [1 .. 6], e <- [1 .. 6], length (group(sort[a,b,c,d,e])) == 2, length (group(sort[a,b,c,d,e]) !! 0 ) /= 4 && length (group(sort[a,b,c,d,e]) !! 1 ) /= 4 ]

straight :: [(Int, Int, Int, Int, Int)]
straight = [(a,b,c,d,e) | a <- [1 .. 6], b <- [1 .. 6], c <- [1 .. 6], d <- [1 .. 6], e <- [1 .. 6], sort [a,b,c,d,e] == [1,2,3,4,5] || sort[a,b,c,d,e] == [2,3,4,5,6]]


toChance :: [(Int, Int, Int, Int, Int)] -> Double
toChance l = (fromIntegral (length l) / fromIntegral 7776) * 100

bust :: Double
bust =  100-(toChance poker + toChance fourKind + toChance threeKind + toChance pair + toChance twoPair + toChance fullHouse + toChance straight)


main :: IO ()
main = do
    print(differentieer (\x->x^2+3*x+5) 0.0001 5)
    print(integreer (\x-> -x^2+4*x+5) 0 5 0.0001)
    print(unique[1,1,1,2,2,3,3,3,3,4,5,7,7,8])
    print(toChance poker)
    print(toChance fourKind)
    print(toChance threeKind)
    print(toChance pair)
    print(toChance twoPair)
    print(toChance fullHouse)
    print(toChance straight)
    print(bust)