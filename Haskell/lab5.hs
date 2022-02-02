main :: IO()
main = do
    print 14
    print v1
    print v2
    print (f3 v1)
    print (f3 v2)
    print $ f3 v1
    print (($) (+ 1) 5)
    print (getClosestPoint [Point2D 1 2, Point2D 3 4] (Point2D 0 0))
    print (getClosestPoint' [Point2D 1 2, Point2D 3 4] (Point2D 0 0))
    print (getClosestPointVector [(-1, 10), (1,2), (3,4)] (0,0))
    print (size t1)
    print (inorder t1)
    print (inorder t2)
    print (mirrorTree t3)


type Vector1 = (Double, String, Int)
v1 :: (Double, String, Int)
v1 = (1.2, "abc", 5)

v2 :: Vector1
v2 = (4.2, "abbc", 5)

f3 :: Vector1 -> Int
f3 p@(_, _, v) = v 


{-Задача 1. Да се дефинира функция getClosestPoint, която приема списък от точки и още една точка p. 
Като резултат функцията да връща тази точка от списъка, която е най-близо до точката p.-}
data Point = Point2D Double Double| Point3D Double Double Double
  deriving (Eq, Show)

distance :: Point -> Point -> Double
distance (Point2D x1 y1)  (Point2D x2 y2) = 
    sqrt ((x1 - x2) * (x1- x2) + (y1-y2) * (y1 - y2))
distance (Point3D x1 y1 z1) (Point3D x2 y2 z2) = 
    sqrt ((x1 - x2) * (x1- x2) + (y1-y2) * (y1 - y2) + (z1 - z2) * (z1 - z2))
distance _ _ = error "Not compatible"

getClosestPoint1 :: [Point] -> Point -> Double
getClosestPoint1 (x:xs) p = helper (distance x p) xs
  where
    helper minD lst@(y:ys)
      | null lst = minD
      | (distance y p) < minD = helper (distance y p) ys
      | otherwise                      = helper minD ys


getClosestPoint :: [Point] -> Point -> Point
getClosestPoint []     _ = error "Empty list"
getClosestPoint [p]    _ = p
getClosestPoint (p:ps) center = helper ps p
    where
        helper [] curClosest = curClosest
        helper (p:ps) curClosest =
            if distance p center < distance curClosest center
            then helper ps p
            else helper ps curClosest


getClosestPoint' :: [Point] -> Point -> Point
getClosestPoint' [] center = error "Empty list"
getClosestPoint' ps c =
    foldl1 (\ p q -> if distance p c < distance q c then p else q) ps



getClosestPointVector :: [(Double,Double)] -> (Double,Double) -> (Double,Double)
getClosestPointVector ps c = snd (minimum [(distance p c, p) | p <- ps])
    where distance (x1, y1) (x2, y2) = sqrt ((x1 - x2) * (x1- x2) + (y1-y2) * (y1 - y2))


{-Задача 2. Да се дефинират рекурсивен алгебричен тип двоично дърво (BTree) и следните функции:
a) функция size, която намира броя на елементите на двоично дърво
b) функция height, която намира височината на двоично дърво
c) функция sumTree, която намира сумата от елементите на двоично дърво
d) функция sumLeaves, която намира сумата елементите по листата на двоично дърво
e) функция inorder, която обхожда двоично дърво в ред Ляво-Корен-Дясно

Примери:
t1 :: BTree                                                         --    5
t1 = Node 5 (Node 2 Empty                              --                / \
                                  (Node 3 Empty Empty))  --            2   6
                     (Node 6 Empty Empty)                --                 \
                                                                          -- 3 
                                          
t2 :: BTree                                                             --    5
t2 = Node 5 (Node 3 Empty Empty)                      --                     / \
                     (Node 4 (Node 5 Empty Empty)      --                   3   4
                                   (Node 7 Empty Empty))     --           /  \
                                                                  --     5    7-}

data BTree = Empty | Node Int BTree BTree
    deriving Show

t1 :: BTree                                --    5
t1 = Node 5 (Node 2 Empty                  --   / \
                    (Node 3 Empty Empty))  --  2   6
            (Node 6 Empty Empty)           --   \
                                           --    3 
                                          
t2 :: BTree                                --    5
t2 = Node 5 (Node 3 Empty Empty)           --   / \
            (Node 4 (Node 5 Empty Empty)   --  3   4
                    (Node 7 Empty Empty))  --     / \
                                           --    5   7

size :: BTree -> Int
size Empty = 0
size (Node _ lt rt) = 1 + size lt + size rt

height :: BTree -> Int
height Empty = 0
height (Node _ lt rt) = 1 + max (height lt) (height rt)

sumTree :: BTree -> Int
sumTree (Node v lt rt) = v + sumTree lt + sumTree rt

sumLeaves :: BTree -> Int
sumLeaves Empty = 0
sumLeaves (Node v Empty Empty) = v
sumLeaves (Node _ lt rt) = sumLeaves lt + sumLeaves rt

--ako imame dvoichno naredeno darvo, inorder shte mi podredi chislata po narastvasht red
inorder :: BTree -> [Int]
inorder Empty = []
inorder (Node v lt rt) = inorder lt ++ [v] ++ inorder rt

--Задача 3. Да се дефинира функция getLevel :: getLevel :: Int -> BTree -> [Int], която намира елементите на k-то ниво на двоично дърво.

getLevel :: Int -> BTree -> [Int]
getLevel _ Empty = []
getLevel 0 (Node v _ _) = [v]
getLevel k (Node _ lt rt) = getLevel (k - 1) lt ++ getLevel (k - 1) rt


--Задача 4. Да се дефинира функция average :: BTree -> Double, която пресмята средно-аритметичното от записаното във върховете на двоично дърво.

average :: BTree -> Double
average Empty = error "Empty tree"
average bt  = fromIntegral (sumTree bt) / fromIntegral (size bt)


{-Задача 5. Да се дефинира функция mirrorTree :: BTree -> BTree, която преобразува дърво в "огледалното" му.
Пример:  

     1                     1
    /  \                 /  \
   2    3     =>        3    2
  /    /  \           /  \    \
 5    7   6           6   7    5

-}

t3 :: BTree                                --     1
t3 = Node 1 (Node 2 (Node 5 Empty Empty)   --    / \
                    Empty)                 --   2   3
            (Node 3 (Node 7 Empty Empty)   --  /   / \
                    (Node 6 Empty Empty))  -- 5   7   6


mirrorTree :: BTree -> BTree
mirrorTree Empty = Empty
mirrorTree (Node v lt rt) = Node v (mirrorTree rt) (mirrorTree lt)