main :: IO()
main = do
    print 1
    print [[],[[3,2,1]]] 
    print (Triangle 3 4 5)
    print (Circle 1.0)
    print (Rectangle 10 15)
    print (perimeter b)
    print (isRound b)
    print (sumArea a)
    print (sumArea1 a)
    print (biggestShape [b])
    print (biggestShape1 [b])
    print (biggestShape a)
    print (biggestShape2 a)
    print (biggestShape1 a)
    print checkEq
    



{- Задача 1. Да се дефинира тип Shape с 3 конструктора: 
Circle,        който има 1 аргумент  - радиус
Rectangle, който има 2 аргумента - ширина и височина
Triangle,    който има 3 аргумента - 3 страни
Нека Shape да е екземпляр на класа Show и за него да се дефинира метода show.-}

data Shape = Circle Double | 
             Rectangle Double Double |
             Triangle Double Double Double
              deriving Show 

a :: [Shape]
a = Circle 10 : [Rectangle 10 21]

{-Задача 2. За Shape да се дефинират:
a) функция perimeter :: Shape -> Double, която намира периметъра на фигурата
b) функция area :: Shape -> Double, която намира лицето на фигурата
c) предикат isRound :: Shape -> Bool, който проверява дали дадена фигура е кръгла-}

perimeter :: Shape -> Double
perimeter (Circle r)      = 3.14 * r * 2
perimeter (Rectangle a b) = 2 * (a + b)
perimeter (Triangle a b c) = a + b + c 

area :: Shape -> Double 
area (Circle r) = pi * r * r 
area (Rectangle a b) = a * b 
area (Triangle a b c) = sqrt (p * (p - a) * (p - b) * (p - c)) 
    where p = (a + b + c) / 2

isRound :: Shape -> Bool 
isRound (Circle _) = True 
isRound _          = False

b :: Shape
b = Circle 10

{-Задача 3. Да се дефинира функция sumArea, която приема списък от фигури и връща сумата от лицата на фигурите в списъка. 
Да се дефинира още една функция biggestShape, която намира фигурата с най-голямо лице.-}

sumArea :: [Shape] -> Double
sumArea shapes = sum (map area shapes)

sumArea1 :: [Shape] -> Double
sumArea1 shapes
    | null shapes = 0
    | otherwise   = area (head shapes) + sumArea1 (tail shapes)

biggestShape1 :: [Shape] -> Shape
biggestShape1 [] = error "No figures"
biggestShape1 shapes = helper (head shapes) (tail shapes)
    where
        helper max lst 
            | null lst                   = max
            | area max < area (head lst) = helper (head lst) (tail lst)
            | otherwise                  = helper max (tail lst)


biggestShape :: [Shape] -> Shape
biggestShape [] = error "Empty list"
biggestShape shapes = foldl1 (\ u v -> if area u > area v then u else v) shapes

biggestShape2 :: [Shape] -> Shape
biggestShape2 (s:ss) = foldl (\ u v -> if area u > area v then u else v) s ss

{-Задача 4. Да се дефинира тип Point, който задава точка в равнината и точка в пространството. 
Нека да е екземпляр на класа Eq и за него да се дефинира равенство на точки от една и съща размерност.-}

data Point = Point2D Double Double | Point3D Double Double Double
    deriving (Eq, Show)

c :: Point 
c = Point2D 2 3

d :: Point
d = Point2D 2.5 3.0

checkEq :: Bool
checkEq = c == d


{-Задача 5. Да се дефинира функция distance за работа с типа, която намира разстоянието между две (съвместими) точки. 
Ако точките са с различна размерност (т.е имат различен брой координати) функцията да връща съобщение за грешка.-}

distance :: Point -> Point -> Double
distance (Point2D x1 y1)    (Point2D x2 y2) =
    sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))
distance (Point3D x1 y1 z1) (Point3D x2 y2 z2) =
    sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2))
distance _ _ = error "Not compatible"

