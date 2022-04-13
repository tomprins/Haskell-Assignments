module Opdracht2 where
import Data.Char

--opdracht 1a
euclid::Integer -> Integer -> Integer 
euclid x y
    | y == 0 = x
    | x > y = euclid y (x `mod` y)
    | y > x = euclid x (y `mod` x)
    | x == y = x

--opdracht 1b
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b =
    let (g, s, t) = egcd (b `mod` a) a
    in (g,(t - (b `div` a) * s), s)

wrapper :: Integer -> Integer ->  (Integer, Integer, Integer)
wrapper a b
    | (a * d) `mod` b == 1 && d > 0 = (c, d, e+b)
    | (b * e) `mod` a == 1 && e > 0 = (c, d+a, e)
    | otherwise = (0, 0, 0)
    where (c,d,e) = egcd a b

--opdracht 2
keyGen :: Integer -> Integer -> (Integer, Integer, Integer)
keyGen p q = (e,d,m)
    where m = p*q
          m2 = (p-1)*(q-1)
          e = eBepalen 2 m2
          (x,d,y) = wrapper e m2
    
eBepalen :: Integer -> Integer -> Integer
eBepalen e m
    | gcd e m == 1 = e
    | otherwise = eBepalen (e+1) m

--opdracht 3a
rsaencrypt :: (Integer, Integer) -> Integer -> Integer 
rsaencrypt (e, m) x = (x^e `mod` m)

--opdracht 3b
rsadecrypt :: (Integer, Integer) -> Integer -> Integer
rsadecrypt (d, m) x = (x^d `mod` m)


-- opdracht 4
asciiEncryption :: Char -> Integer -> Integer -> Integer
asciiEncryption ascii a b = rsaencrypt (e, m) (toInteger (ord ascii))
    where (e,d,m) =  keyGen a b

asciiDecryption :: Integer -> Integer -> Integer -> Char
asciiDecryption a b c = chr (fromIntegral (rsadecrypt (d ,m) c))
    where(e,d,m) = keyGen a b

-- opdracht 5
-- Als Alice en Bob hun berichten versleutelen met behulp van asymetrische encryptie,
-- dan kan alleen bob's private key, het bericht openen en voor de rest geen enkele andere sleutel (zelfs die van alice niet).
-- Dit kan bereikt worden met behulp RSA encrypion, die een publieke en private sleutel voor beide personen genereert.
-- Vervolgens hoeft Alice haar bericht alleen te versleutelen met bob's publieke sleutel,
-- en dan kan alleen bob het bericht openen met zijn eigen private sleutel.
-- zo kan niemand het bericht lezen behalve bob.

main :: IO ()
main = do
    print(egcd 14 42)
    print(egcd 14 42)
    print(keyGen 534 534)
    print(eBepalen 0 1)
    print(gcd 39 40)
    print(egcd 27 28)
    print(rsaencrypt (5, 91) 75)
    print(rsaencrypt (5, 91) 17)
    print("asciiEncryption", asciiEncryption 'K' 7 13)
    print("asciiDecryption", asciiDecryption 7 13 17)
    print(keyGen 7 13)