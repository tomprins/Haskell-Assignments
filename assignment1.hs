module Opdracht1 where
import Data.Bits

--1a
faca :: Int -> Int 
faca 0 = 1 
faca x = x * faca ( x - 1 )

--1b
facb :: Int -> Int 
facb x
    | x == 0 = 1
    | otherwise = x * facb (x - 1)

--2a
nulpuntena :: Double -> Double -> Double -> [Double]
nulpuntena a b c
    | sqrt(b^2-4*a*c) < 0 = []
    | sqrt(b^2-4*a*c) == 0 = [(-b + sqrt(b^2-4*a*c)) / (2*a)]
    | otherwise = [(-b + sqrt(b^2-4*a*c)) / (2*a), (-b - sqrt(b^2-4*a*c)) / (2*a)]

--2b
nulpuntenb :: Double -> Double -> Double -> [Double]
nulpuntenb a b c
    | discriminant < 0 = []
    | discriminant == 0 = [(-b + discriminant) / (2*a)]
    | otherwise = [(-b + discriminant) / (2*a), (-b - discriminant) / (2*a)]
    where discriminant = sqrt(b^2-4*a*c)

--2c
dobbelstenen5 :: [(Int, Int, Int)]
dobbelstenen5 = [(a,b,c) | a <- [1 .. 6], b <- [1 .. 6], c <- [1 .. 6], (a+b+c) `mod` 5 == 0]

--2d
dobbelstenenN :: Int -> [(Int, Int, Int)]
dobbelstenenN n = [(a,b,c)| a <- [1 .. 6], b <- [1 .. 6], c <- [1 .. 6], (a+b+c) `mod` n == 0]

--3
riddle :: [(Float, Float, Float)]
riddle = [(a,b,c) | a <- [-100 .. 100], b <- [-100 .. 100], c <- [-100 .. 100], b == a*c, a == 2*(b-c), c == 0.5*(a+b)]

--4a
mult :: Int -> Int -> Int
mult a b
    | b == 0 = 0
    | otherwise = a + mult a (b-1)

--4b
fastmult :: Integer  -> Integer -> Integer
fastmult a b
   | b < 1 = 0
   | b == 2 = shiftL a 1
   | b  `mod` 2 == 1 = fastmult a (b-1) + a
   | otherwise = fastmult (shiftL a 1) (shiftR b 1)

--5a
pow :: Int -> Int -> Int
pow a b
    | b <= 0 = 1
    | otherwise = a * pow a (b-1)

--5b
fastpow :: Integer -> Integer -> Integer
fastpow a b
    | b <= 0 = 1
    | otherwise = fastmult a (fastpow a (b-1))


main :: IO ()
main = do
    print("1a",faca 3)
    print("1b",facb 3)
    print("2a",nulpuntena 1 5 3)
    print("2b",nulpuntenb 1 5 3)
    print("2c",dobbelstenen5)
    print("2d",dobbelstenenN 8)
    print("3 ",riddle)
    print("4a", mult 5 10)
    -- print("4a", mult 4611686018427387903 2)
    print("4b", fastmult 8 5)
    print("5a", pow 4 2)
    -- print("5a", pow 3037000499 2)
    print("5b", fastpow 4 2)
