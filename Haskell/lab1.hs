main :: IO()
main = do
    print 10
    print (mymin 7 15)
    print (10 `mymin` 15)
    print (mymin (-2) 15)
    print (isInside 10 1 15)
    print (myfunc 10 20)
    print (myfunc' 10 20)
    print (myfib 5)
    print (myfib' 5)
    print (myFibIter 5)
    print (round (fromIntegral a / fromIntegral b)) 
    print (gcdd 8 24)
    print (myMaxDiv 24)
    print (sumOdd 1 10)
    print (isPrime 2)
    print (isPrime 25)
    print (countPal 1 100)
    print (countPalindromes 1 100)
    print (1 `countpalindromes` 100)
    print (sumOdds 2 8)

{- Задача 1. Да се напише функция mymin, която приема два аргумента и връща по-малкия от тях.-}
mymin :: Int -> Int -> Int
mymin a b = if a < b then a else b

{-Задача 2. Да се дефинира функцията isInside x a b, която проверява дали числото x се намира в затворения интервал [a, b].-}
isInside :: Double -> Double -> Double -> Bool
isInside x a b = a <= x && x <= b
{-Задача 3. Да се напише функция myfunc, която пресмята средно аритметично на квадратите на 2 числа.
-}

myfunc :: Double -> Double -> Double
myfunc a b = (a * a + b * b) / 2

myfunc' :: Double -> Double -> Double
myfunc' a b = average (square a) (square b)
    where 
        square x = x * x 
        average a b = (a + b) / 2

{-Задача 4. Да се напише myfib, която получава един аргумент n и връща n-тото число на Фибоначи. 
Да се напише и итеративно решение
(Заб.: редицата е 1, 1, 2, 3, 5, ... и е индексирана от 0.)-}

myfib :: Int -> Int
myfib n
    | n == 0 || n == 1  = 1
    | otherwise   = myfib (n - 1) + myfib (n - 2)

myfib' :: Int -> Int
myfib' 0 = 1
myfib' 1 = 1
myfib' n = myfib' (n - 1) + myfib' (n - 2)

myFibIter :: Integer -> Integer
myFibIter n = helper 1 1 0
    where 
        helper a b i
            | i == n    = a
            | otherwise = helper b (a + b) (i + 1)



a :: Int
a = 10
b :: Int
b = 2
{-Задача 5. Да се напише функция mygcd a b, която връща НОД(a, b).-}

gcdd :: Int -> Int -> Int
gcdd a b
    | a == 0 = b
    | b == 0 = a
    | a == b = a
    | a > b = gcdd (a - b) b
    | otherwise = gcdd a (b - a)

mygcd :: Int -> Int -> Int
mygcd a b
    | a == b    = a
    | a > b     = mygcd (a - b) b
    | otherwise = mygcd a (b - a)

{-Задача 6. Да се напише функция mymaxdivisor x, която намира най-големия делител d на цялото число x > 1, за който d < x.-}
myMaxDiv :: Int -> Int
myMaxDiv x = helper (x - 1)
    where
        helper i
            --  | i <= 0        = 1
            | mod x i == 0  = i
            | otherwise     = helper (i - 1)
{-Задача 7. Да се дефинира функция, която намира сумата на нечетните числа в затворения интервал [a, b].-}

sumOdd :: Integer -> Integer -> Integer
sumOdd a b
    | a > b         = 0
    | mod a 2 == 1  = a + sumOdd (a + 2) b
    | otherwise     = sumOdd (a + 1) b


sumOdds :: Int -> Int -> Int
sumOdds a b
    | even a         = helper 0 (a + 1)
    | otherwise      = helper 0 a
        where 
            helper sum cur
                | cur > b  = sum
                | otherwise = helper (sum + cur) (cur + 2)


{-Задача 8. Да се дефинира предикат, който проверява дали естественото число n е просто.-}
isPrime1 :: Int -> Bool
isPrime1 n
    | n == 1 = False
    | n == 2 = True 
    | otherwise = helper 2
        where 
            helper d
                | d == n = True
                | mod n d == 0 = False
                | otherwise    = helper (d + 1)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = helper 2
    where
        helper d
            | d == n         = True
            | n `mod` d == 0 = False
            | otherwise      = helper (d + 1)

            

{-Задача 9. Да се дефинира функция, която намира броя на палиндромите в интервала [a, b], където a и b са цели неотрицателни числа и a<b.-}
isPal :: Int -> Bool 
isPal n = n == myreverse n 0
        where 
            myreverse n sum = 
                if n <= 0
                then sum 
                else myreverse (n `div` 10) (sum * 10 + mod n 10)

countPal :: Int -> Int -> Int 
countPal a b
    | a > b = 0
    | isPal a = 1 + countPal (a + 1) b
    | otherwise = countPal (a + 1) b


countPalindromes :: Int -> Int -> Int
countPalindromes a b
    | a > b          = 0
    | isPalindrome a = 1 + countPalindromes (a + 1) b
    | otherwise      = countPalindromes (a + 1) b
    where
        reverseHelper k res =
            if k < 10
            then res * 10 + k
            else reverseHelper (k `div` 10) (res * 10 + k `mod` 10)
        isPalindrome elem = reverseHelper elem 0 == a


countpalindromes :: Int -> Int -> Int 
countpalindromes a b 
    | a > b = 0 
    | ispalindome a 0 = 1 + countpalindromes (a + 1) b 
    | otherwise = countpalindromes (a + 1) b 
        where 
            ispalindome y x = 
                if y == 0 
                then x == a 
                else ispalindome (div y 10) (x * 10 + mod y 10)

{-Задача 10. Да се дефинира функция, която чрез линейно итеративен процес намира броя на естествените делители на едно естествено число.-}
countDivs :: Int -> Int
countDivs n = helper 1 2
    where 
        helper count d
            | d > n = count
            | mod n d == 0 = helper (count + 1) (d + 1)
            | otherwise    = helper count (d + 1)


countDivisors :: Int -> Int
countDivisors n = helper 1 0
    where
        helper d result
            | d > n        = result
            | mod n d == 0 = helper (d + 1) (result + 1)
            | otherwise    = helper (d + 1) result