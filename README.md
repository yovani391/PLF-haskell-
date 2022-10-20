# PLF-haskell-
Class assignments
There are assignments class for unit

--> Unit I: Conceptos fundamentales

--> Unit II: Programación funcional

-- 
-- Code listas
--
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
