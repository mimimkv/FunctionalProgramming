main :: IO()
main = do
    print [1, 2, 3]
    print v1
    print (null v1)
    print v2
    print (head v2)
    print (tail v2)
    print (1 : [])
    print [v2]
    print (1 : [2, 3])
    print ([1] ++ [2, 3])
    print ([1, 2, 3] ++ [4])
    print v3
    print v4
    print (incrementAllBy [1, 2, 3] 1)
    print (incrementAllBy' [1, 2, 3] 1)
    print (incrementAllBy1 [1, 2, 3] 2)
    print [2 * x| x <- [1..10]]
    print [2 * x| x <- [1..10], even x]
  
    print [(x, y) | x <- [1, 2, 3], y <- [4,5]]
    print [(x, y) | x <- [1, 2, 3], y <- [4,5], x + y > 5]



    

v1 :: [Int]
v1 = []

v2 :: [[Int]]
v2 = [[1, 2, 3], [4, 5], []]

sum1 :: [Int] -> Int 
sum1 xs =
    if null xs then 0 else head xs + sum1 (tail xs)

sum2 :: [Int] -> Int 
sum2 [] = 0
sum2 (x:xs) = x + sum2 xs


sum3 :: [Int] -> Int 
sum3 []         = 0
sum3 (x1:x2:_:xs) = x1 + x2 + sum3 xs
sum3 (x:xs)     = x + sum3 xs

sum4 :: [Int] -> Int 
sum4 []           = 0
sum4 ps@(x1:x2:x3:xs) = x1 + x2 + sum4 ps
sum4 (x:xs)       = x + sum4 xs



--function that works with both double and int numbers:
sum5 :: (Num t, Eq t, Ord t) => [t] -> t
sum5 []     = 0
sum5 (x:xs) = x + sum5 xs


v3::(Int, Double, Int)
v3 = (1, 1.2, 3)

v4 :: [(Int, Double)]
v4 = [(1, 1.2), (2, 2.4)]

--Задача 1. Да се дефинират следните функции:

--incrementAllBy :: [Int] -> Int -> [Int], която получава списък и число и го добавя към всеки елемент на списъка

incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy xs n = 
    if null xs 
    then [] 
    else (head xs + n) : incrementAllBy (tail xs) n


incrementAllBy' :: [Int] -> Int -> [Int]
incrementAllBy' []     _ = []
incrementAllBy' (x:xs) a = (x + a) : incrementAllBy' xs a

incrementAllBy1 :: [Int] -> Int -> [Int]
incrementAllBy1 xs a = [x + a | x <- xs]

--multiplyAllBy :: [Int] -> Int -> [Int], която получава списък и число и умножава всеки елемент на списъка по числото

multiplyAllBy :: [Int] -> Int -> [Int]
multiplyAllBy xs a = [x * a | x <- xs]

--filterSmallerThan , която получава списък и число и премахва елементите на списъка, които са по-малко от числото

filterSmallerThan :: [Int] -> Int -> [Int]
filterSmallerThan []     _ = []
filterSmallerThan (x:xs) a =
    if x < a
    then filterSmallerThan xs a
    else x : filterSmallerThan xs a

filterSmallerThan' :: [Int] -> Int -> [Int]
filterSmallerThan' xs a
    | null xs = []
    | head xs < a = filterSmallerThan' (tail xs) a
    | otherwise = head xs : filterSmallerThan' (tail xs) a


filterSmallerThan'' :: [Int] -> Int -> [Int]
filterSmallerThan'' xs a = [x | x <- xs, x >= a]


{-Задача 2. Да се дефинира функция isAscending :: Integer -> Bool, която проверява дали цифрите на число са във възходящ ред
Функцията да получава число, но да работи със списък от цифрите му.-}

isAscending' :: [Int] -> Bool
isAscending' xs
    | null xs || null (tail xs) = True
    | head xs >= head (tail xs) = False
    | otherwise = isAscending' (tail xs)

isAscending'' :: [Int] -> Bool
isAscending'' []         = True
isAscending'' [_]        = True
isAscending'' (x1:x2:xs) = x1 < x2 && isAscending'' (x2:xs)

numToList :: Int -> [Int]
numToList n =
    if n < 10
    then [n]
    else (n `mod` 10) : numToList (n `div` 10)

isAscending :: Int -> Bool
isAscending n = isAscending' (reverse (numToList n))

isAscending1 :: Int -> Bool 
isAscending1 n = helper (toList n)
    where 
        toList n = 
            if n <= 0 
            then [] 
            else mod n 10 : toList (div n 10)
        helper [x] = True 
        helper (x:y:xs) = 
            if x < y 
            then False 
            else helper (y:xs)


isAscending2 :: Int -> Bool 
isAscending2 num = checkAscending (numList num) 
    where 
        numList num = 
            if num < 10 
            then [num] 
            else mod num 10 : numList (div num 10) 
        checkAscending lst 
            | null (tail lst) = True 
            | head lst >= head (tail lst) = checkAscending (tail lst) 
            | otherwise = False
    