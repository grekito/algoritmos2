esCero :: Int -> Bool
esCero x = x==0

esPositivo :: Int -> Bool
esPositivo x = x > 0

esVocal :: Char -> Bool
esVocal x = x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u'

valorAbsoluto :: Int -> Int
valorAbsoluto x | x >= 0 = x
                | x < 0 = (-x)

-- Laboratorio 2
--a 
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = x == True && paratodo xs

--b 
sumatoria :: [Int] -> Int 
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--c
productoria :: [Int] -> Int 
productoria [] = 1
productoria (x:xs) = x * productoria xs

--d 
factorial :: Int -> Int 
factorial 0 = 1
factorial 1 = 1
factorial x = x * (x-1) * factorial (x-2)

--e 
promedio :: [Int] -> Int 
promedio [] = 0 
promedio (xs) = div (sumatoria (xs)) (length (xs))

-- Laboratorio 3

--a 
expresion1 :: [Int] -> Bool
expresion1 [] = True
expresion1 (x:xs) = x > 0 && expresion1 xs

--b 
expresion2 :: [Int] -> Int -> Bool
expresion2 [] x = False
expresion2 (y:ys) x = y == x || expresion2 ys x

--c 

existe :: Int -> [Int] -> Bool
existe y [] = False
existe y (x:xs) = y == x || existe y xs

expresion3 :: [Int] -> [Int] -> Bool
expresion3 [] [] = False
expresion3 (y:ys) [] = False
expresion3 [] (x:xs) = False
expresion3 (y:ys) (x:xs) = existe y (x:xs) || expresion3 ys (x:xs)

--d igual al adyacente 
expresion4 :: [Int] -> Bool
expresion4 [] = True
expresion4 (x:y:xs) = x == y && expresion4 xs
