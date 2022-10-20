module Practica1 where 

{-
--1st
average3Numbers :: Float -> Float-> Float -> Float
average3Numbers x y z = (x+y+z) / (3)
--2nd
--isLastDigit3
--3rd
has3Digits :: Int -> Bool
has3Digits a = if ((a > 99) && (a < 1000)) then True else False
--4th
isNegative ::Int -> Bool
isNegative numero= if numero<0 then True else False
--5th (sum2Digits)
addTwoDig :: Int -> Int
addTwoDig x 
    | (x > 9) && (x < 100) = (x `mod` 10) + ((x `div` 10) `mod` 10) 
    | (x > 99) = error "No posee 2 cifras"
--6th
even2Digit :: Int -> Bool
even2Digit x 
    | ((x > 9) && (x < 100)) = even (x `mod` 10) && even ((x `div` 10) `mod` 10) 
    | (x > 99) = error "Number dowsn´t have two digit"
--7th (isPrimeNumber)
nPrimo :: Int -> Bool
nPrimo n = elem n [2,3,5,7,11,13,17,19]
--8th (isEvenAndPrimeNumber )
nPrimo2 :: Int -> Bool
nPrimo2  n = nPrimo n && even (n)
--9th (isMultiple)
multN :: Int -> Int -> Bool
multN n m = if (n `mod` m) == (0) then
    True else False
--10
--11
--12th (isEvenSum2Number)
itsEven :: Int -> Int -> Bool
itsEven n m = if (((n + m) `mod` 2) == 0) then
    True else False
--13
--14
--15th (equal3Digits)
isEq :: Int-> Bool
isEq x
    | ((x < 100)) = error "Number doesn't have tree elements"
    | ((x > 99) && (x < 1000)) = if ((x `mod` 10) == ((x `div` 10) `mod` 10)) then
        True else if (((x `mod` 10)) == ((x `div` 100) `mod` 10)) then
            True else if (((x `div` 10) `mod` 10 )== ((x `div` 100) `mod` 10)) then
                True else False
--16
--17
--18
--19ht ()
--20th
xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not (x && y)
-}

{-
Listas --> let lista = [lo que vamos a mostrar | x <- [lista de la que filtraremos], condiciones]
...
Funciones con listas
...
-}

--  Ejercicicio 1:  firstToEnd
firstToEnd::([x]) -> [x] 
firstToEnd [] = error "Lista vacía"
firstToEnd [x] = error "Solo un elemento"
firstToEnd xs = tail xs ++ [head xs]

--  Ejercicicio 2:  mindAndMax
minAndMax::(Ord a) => [a] -> [a]
minAndMax [] = error "Lista sin elementos!"
minAndMax [x] = error "Sólo un elememto!"
minAndMax (xs) = [minimum xs] ++ [maximum xs]

--  Ejercicicio 3: minorsFirstElement
minorsFirstElement::(Ord a) => [a] -> [a]
minorsFirstElement [] = error "Lista sin elementos!"
minorsFirstElement [x] = error "Sólo un elemento!"
minorsFirstElement (xs) = filter (<head xs) xs

--  Ejercicicio 4:  greaterOrEqualFirstElement
greaterOrEqualFirstElement::(Ord a) => [a] -> [a]
greaterOrEqualFirstElement [] = error "Sin elementos!"
greaterOrEqualFirstElement [x] = error "Lista con único elemento!"
greaterOrEqualFirstElement (xs) = filter (>head xs) xs

--  Ejercicicio 5:  minorsToSumFirstAndSecondElem
minorsToSumFirstAndSecondElem::(Integral a) => [a] -> [a]
minorsToSumFirstAndSecondElem [] = error "Lista sin elementos!"
minorsToSumFirstAndSecondElem [a] = error "Elemento único!"
minorsToSumFirstAndSecondElem list = [x| x <- tail (tail list), x <head list + head(tail list)]

--  Ejercicio 6:  listSumDuplaToList
listSumDuplaToList::(Integral a) => [(a,a)] -> [a]
listSumDuplaToList [] = []
listSumDuplaToList list = fst(head list) + snd (head list) : listSumDuplaToList (tail list)

--  Ejercicio 7:    listMultTripleToList
listMultTripletaToList::(Num a) => [(a,a,a)] -> [a]
listMultTripletaToList [] = []
listMultTripletaToList xs = [x*y*z| (x,y,z) <- xs]

--  Ejercicicio 8:  changeFstToSnd
changeFstToSnd::[(a,a)] -> [(a,a)]
changeFstToSnd [] = []
changeFstToSnd xs = [(y,x) | (x,y) <- xs]

--  Ejercicicio 9:  sumVectors
sumVectors::(Num a) => [(a,a)] -> (a,a)
sumVectors [] = error "lista vacia"
sumVectors xs = (sum [ x | (x,y) <- xs], sum [ y | (x,y) <- xs])

--  Ejercicicio 10: dividers
dividers::Integral a => a -> [a]
dividers n = [x| x <- [1..n], (mod n x) == 0]

--  Ejercicicio 11: primeNumbers
primeNumbers::Integral a => a -> [a]
primeNumbers n = [x | x <- [1..n], [1,x] == (dividers x)]

--  Ejercicicio 12: infinitePrimeNumbers
infinitePrimeNumbers::Integral a => [a]
infinitePrimeNumbers = [x | x <- [1..], [1,x] == (dividers x)]