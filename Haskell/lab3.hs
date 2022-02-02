main :: IO()
main = do
    print 1
    print (isImage [1, 2, 3] [3, 4, 5])
    print (isImage [1, 2, 3] [3, 2, 5])
    print ((\ a b c -> a * 2 + b * c) 1 2 3)
    print (f1 (\ x-> 5 * x))
    print (f2 10)
    print (f1 f2)
    print ((f5 1 2) 3)
    print (f5 1 2 3)
    print (((f5 1) 2) 3)
    print (f6'' 3)
    print ((mycompose' (\ x -> x * 2) (\ x -> x + 1)) 5)
    print (((\ x -> x * 2) . (\ x -> x + 1)) 5)
    print ((* 2) 4)
    
    print (((-) 2) 4)
    print ((2 -) 4)
    print (((* 2) . (+ 1) . (* 3)) 5)
    print (mymap (* 2) [1..5])
    print (map (* 2) [1..5])
    print (zip [9, 4, 1] [1..])
    print (zip [9, 4, 1] [0..])
    print (zip' [9, 4, 1] [1..])
    print (map (\ (a, b) -> a * b) (zip [9, 4, 1] [1..]))
    print (map ((+ 1) . (* 2)) [1..10])
    print (chunksOf 4 [1..15])
    print (isTriangular [[1,2,3],[0,4,5],[0,0,6]])
    print (isTriangular [[1,2,3],[0,4,5],[1,-1,6]])
    print (divisors 15)
    print (primesInRange 1 100)
    print (isZero [1,2,3] 0)
    print (isZero [0, 1, 4] 1)
    print (isZero [0, 0, 1] 2)
    print (isZero [0, 0, 0] 2)
    print (isZero [0, 0, 0] 0)
    print (isZero [0, 0, 0] 1)
    print (isZero [0, 0, 0, 1] 3)
    print (isZero [1, 0, 0] 2)
    print (isZero [1, 0, 0] 1)
    print (isTriangular1 [[1,2,3],[0,4,5],[0,0,6]])
    print (isTriangular1 [[1,2,3],[0,4,5],[1,-1,6]])
    print (isTriangular1 [[1,2, 3], [0, 5, 6], [0, 0, 9]])-- ; -> #t
    print (isTriangular1 [[0, 2, 3], [0, 0, 6], [1 ,0 ,0]])--; -> #f
    print (isTriangular1 [[1, 2, 3], [1, 5, 6], [0, 0, 9]])-- ; -> #f
    print (isTriangular1 [[1, 2, 3, 4], [0, 5, 6, 7], [0, 0 ,8, 9], [0, 0, 0, 9]])--; -> #t
    print (sumD 6)
    print (prodSumDiv [1..25] 6)
    print (prodSumDiv2 [1..25] 6)
    print (prodSumDiv1 [1..25] 6)
    print (prodSumDivF [1..25] 6)
    print (isSorted1 [1,1, 2, 2, 3, 4,5])
    print (isSorted1 [-1, 0, 1, 2])
    print (isSorted1 [3, 5, 8,12, 1])
    print (merge [1,2,7,8] [2,3,10])
    print (merge1 [1,2,7,8] [2,3,10])
    print (insert 7 [1, 2, 3, 4, 5, 6 ,8])
    print (insertionSort [1,2,10,3,7,6,5])
    print (reverse [1,2,3,5,0])

f1 :: (Int-> Int) -> Int
f1 func = func 2

f2 :: (Int -> Int)
f2 = \ x -> 2 * x
f3 :: Int -> Int
f3 = \ x -> 2 * x

f4 :: Int -> Int
f4 x = 2 * x


f5 :: Int -> Int -> (Int -> Int)
f5 a b c = a + b + c


f6 :: Int -> Int 
f6 x = f5 1 2 x

f6' :: Int -> Int 
f6' = \ x ->  f5 1 2 x


f6'' :: Int -> Int 
f6'' = f5 1 2


f7 :: (Int -> (Int -> Int)) -> (Int -> Int) -> Int -> (Int -> Int)
f7 func1 func3 x z = 2


mycompose :: (a -> b) -> (c -> a) -> (c -> b)
mycompose f g = \ x -> f (g x)

mycompose' :: (a -> b) -> (c -> a) -> (c -> b)
mycompose' f g x = f (g x)

mycompose'' :: (a -> b) -> (c -> a) -> (c -> b)
mycompose'' f g = f . g

mymap :: (a -> b) -> [a] -> [b]
mymap _ []  = []
mymap f (x:xs) = f x : mymap f xs

zip' :: [a] -> [b] -> [(a, b)]
zip' []     _      = []
zip' _      []     = []
zip' (a:as) (b:bs) = (a, b) : zip' as bs


--Задача 1. Нека as = [a1, a2 … , ak] и bs = [b1, b2 … , bk] са непразни списъци с еднакъв брой числа.
--Да се дефинира предикат isImage :: [Int] -> [Int] -> Bool, който да връща „истина“ точно когато съществува 
--такова число x, че ai = x + bi за всяко i = 1,..., k.

isImage :: [Int] -> [Int] -> Bool 
isImage [] [] = True 
isImage [a] [b] = True 
isImage (a1:a2:as) (b1:b2:bs) = a1 - b1 == a2 - b2 && isImage (a2:as) (b2:bs)

isImag :: ([Int] -> ([Int] -> Bool))
isImag [_]        [_]        = True
isImag (a1:a2:as) (b1:b2:bs) = a1 - b1 == a2 - b2 && isImage (a2:as) (b2:bs)

isImage' :: [Int] -> [Int] -> Bool 
isImage' (a1:as) (b1:bs) = and (map (\ (a, b) -> a - b == a1 - b1) (zip as bs))



--Задача 2. Да се дефинира функция chunksOf :: Int -> [a] -> [[a]], която разделя входния списък на 
--подсписъци с дължина равна на подаденото число.

chunksOf :: Int -> [t] -> [[t]]
chunksOf _ [] = []
chunksOf k xs = take k xs : chunksOf k (drop k xs) 

--Задача 3. Да се дефинира предикат isTriangular :: [[Int]] -> Bool, който получава квадратна числова матрица, 
--представена като списък от списъци, и проверява дали тя е горно триъгълна, т.е. дали всичките елементи под главния ѝ диагонал са нули.


isTriangular :: [[Int]] -> Bool 
isTriangular []   = False 
isTriangular [[_]] = True
isTriangular mat = all (== 0) (tail (map head mat)) && isTriangular (tail (map tail mat))


isZero :: [Int] -> Int -> Bool
isZero xs n = helper xs 1
    where
        helper lst i
            | i > n = True 
            | head xs == 0 = helper (tail lst) (i + 1)
            | otherwise    = False 


isTriangular1 :: [[Int]] -> Bool 
isTriangular1 xs = helper xs 0
    where 
        helper lst i 
            | null lst            = True 
            | isZero (head lst) i = helper (tail lst) (i + 1) 
            | otherwise           = False 


--Задача 4. Да се дефинира функция divisors :: Integer -> [Integer], която генерира списък от всички (собствени) делители на дадено число.

divisors :: Integer -> [Integer]
divisors n = [d | d <- [1..(n-1)], mod n d == 0]


divisorss :: Int -> [Int] 
divisorss n = helper 1
    where 
        helper d
            | d > n        = []
            | mod n d == 0 = d : helper (d + 1)
            | otherwise    = helper (d + 1)

--Задача 5. Да се дефинира функция primesInRange :: Integer -> Integer -> [Integer],. която конструира списък от простите числа в интервала [a,b]

primesInRange2 :: Int -> Int -> [Int]
primesInRange2 a b
    | a > b = []
    | isPrime a 2 = a : primesInRange2 (a + 1) b
    | otherwise   = primesInRange2 (a + 1) b
        where 
            isPrime n d
                | n == 1 = False 
                | d >= n = True 
                | mod n d == 0 = False 
                | otherwise   = isPrime n (d + 1)

primesInRange1 :: Integer -> Integer -> [Integer] 
primesInRange1 a b = [x | x <- [a..b], prime x 2] 
    where 
        prime x k 
            | k >= x = True
            | mod x k == 0 = False 
            | otherwise = prime x (k + 1)

isPrime :: Integer -> Bool 
isPrime n = [1, n] == [d | d <- [1..n], mod n d == 0]

primesInRange :: Integer -> Integer -> [Integer]
primesInRange a b = [n | n <- [a..b], isPrime n]

isPr :: Int -> Bool 
isPr n = all (\d -> mod n d /= 0) [2..(n-1)]

--Задача 6. Да се дефинира функция prodSumDiv :: [Integer] -> Integer -> Integer, която намира произведението на естествените числа в даден 
--списък, сумата от делителите на които е кратна на k.

prodSumDiv :: [Integer] -> Integer -> Integer
prodSumDiv xs k = product [x | x <- xs, mod (sum (divisors x)) k == 0]


prodSumDivF :: [Integer] -> Integer -> Integer
prodSumDivF xs k = foldl (*) 1 [x | x <- xs, mod (sum (divisors x)) k == 0]

prodSumDiv1 :: [Int] -> Int -> Int 
prodSumDiv1 xs k = product [x | x <- xs , mod (divisorsSum x) k == 0]

divisorsSum :: Int->Int 
divisorsSum n = helper 0 1 
    where 
        helper sum i 
            | i >= n = sum 
            | mod n i == 0 = helper (sum + i) (i+1) 
            | otherwise = helper sum (i+1)


prodSumDiv2 :: [Integer] -> Integer -> Integer
prodSumDiv2 xs k
    | null xs  = 1
    | mod (sumD (head xs)) k == 0 = (head xs) * prodSumDiv2 (tail xs) k
    | otherwise                   = prodSumDiv2 (tail xs) k

sumD :: Integer -> Integer
sumD n = helper 1
    where 
        helper d
            | d == n  = 0
            | mod n d == 0 = d + helper (d + 1)
            | otherwise    = helper (d + 1)


--Задача 7. Да се дефинира функция isSorted :: [Int] -> Bool, която проверява дали списък е сортиран във възходящ ред

isSorted1 :: [Int] -> Bool
isSorted1 [] = True
isSorted1 [_] = True
isSorted1 xs = 
    if (head xs) > head (tail xs)
    then False
    else isSorted1 (tail xs)

isSorted2 :: [Int] -> Bool
isSorted2 [] = True
isSorted2 [x] = True
isSorted2 [x1, x2] = x1 <= x2
isSorted2 (x1:x2:xs) = x1 <= x2 && isSorted2 (x2 : xs)


isSorted :: [Int] -> Bool
isSorted xs = all (uncurry (<=)) (zip xs (tail xs))


--Задача 8. Да се дефинира функция merge :: [Int] -> [Int] -> [Int], която получава два сортирани списъка и ги обединява така, че резултатът също да е сортиран

merge :: [Int] -> [Int] -> [Int]
merge as [] = as
merge [] bs = bs
merge xs@(a:as) ys@(b:bs) =
    if a <= b
    then a : merge as ys
    else b : merge xs bs


merge1 :: [Int] -> [Int] -> [Int]
merge1 [] ys = ys
merge1 xs [] = xs
merge1 xs ys
    | head xs < head ys = head xs : merge1 (tail xs) ys
    | otherwise         = head ys : merge1 xs (tail ys)

--Задача 9. Да се дефинира функция insert :: Int -> [Int] -> [Int], която добавя елемент в сортиран списък, като резултатният списък също е сортиран

insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n xs =
    if n < head xs
    then n : xs
    else head xs : insert n (tail xs)


insert1 :: Int -> [Int] -> [Int]
insert1 num [] = [num]
insert1 num xs@(c:cs) =
    if c > num
    then num:xs
    else c : insert num cs


--Задача 10. Да се реализира функция insertionSort :: [Int] -> [Int], която реализира сортиране чрез вмъкване върху списък

insertionSort :: [Int] -> [Int]
insertionSort cs = helper [] cs
    where
        helper result [] = result
        helper result (c:cs) = helper (insert c result) cs


insertionSort1 :: [Int] -> [Int]
insertionSort1 xs = foldr insert [] xs

insertionSort2 :: [Int] -> [Int]
insertionSort2 = foldr insert []