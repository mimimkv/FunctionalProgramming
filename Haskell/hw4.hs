main :: IO()
main = do
    print (countInteresting t1) -- -> 2 (4=2^2, 1=2^0)
    print (countInteresting t2) -- -> 3 (4=2^2, 2=2^1, 1=2^0)
    --print (countChildren t1)
    --print (countChildren t2)
    
    print (getAverageBalance (accounts1,people1) (\ (_,_,city) -> city == "Burgas")) -- -> 24.95 (24.950000000000003)
    print (getAverageBalance (accounts1,people1) (\ (_,n:_,_) -> n =='P')) -- -> 18.85
    print (averageBalanceOfCities (accounts1,people1) ["Sofia","Gabrovo","Stara Zagora"]) -- -> 67.85)


--task 1
type Account = (Int, Int, Double)

accounts1 :: [Account]
accounts1 = [(1,1,12.5),(2,1,123.2),(3,2,13.0),(4,2,50.2),(5,2,17.2),(6,3,18.3),(7,4,19.4)]

type Person = (Int, String, String)

people1 :: [Person]
people1 = [(1,"Ivan","Sofia"),(2,"Georgi","Burgas"),(3,"Petar","Plovdiv"),(4,"Petya","Burgas")]

getBalance :: Account -> Double 
getBalance (_, _, m) = m

getIdFromAcc :: Account -> Int
getIdFromAcc (_, id, _) = id 

getID :: Person -> Int
getID (id, _, _) = id 

--returns all IDs of the people that satisfy the condition p
getPeople :: [Person] -> (Person -> Bool) -> [Int] 
getPeople lst p
    | null lst          = []
    | p (head lst)      = getID (head lst) : getPeople (tail lst) p 
    | otherwise         = getPeople (tail lst) p 

--calculates the sum of all accounts of the people that satisfy the condition
getAllBalance :: [Account] -> [Int] -> Double
getAllBalance accounts ids
    | null accounts                           = 0
    | elem (getIdFromAcc (head accounts)) ids = getBalance (head accounts) + getAllBalance (tail accounts) ids
    | otherwise                               = getAllBalance (tail accounts) ids

--counts all accounts of the people that satisfy the condition
countAccounts :: [Account] -> [Int] -> Double 
countAccounts accounts ids 
    | null accounts                           = 0
    | elem (getIdFromAcc (head accounts)) ids = 1 + countAccounts (tail accounts) ids
    | otherwise                               = countAccounts (tail accounts) ids


getAverageBalance :: ([Account],[Person]) -> (Person -> Bool) -> Double
getAverageBalance (accounts, people) p = getAllBalance accounts pID  / counter 
    where 
        pID = getPeople people p 
        counter = countAccounts accounts (getPeople people p)
       

getCity :: Person -> String
getCity (_, _, city) = city

-- it does not matter if some of the cities repeat
allCities :: [Person] -> [String]
allCities people
    | null people = []
    | otherwise   = getCity (head people) : allCities (tail people)

averageBalanceOfCities :: ([Account],[Person]) -> [String] -> Double
averageBalanceOfCities  (accounts, people) cities
    | null cities                              = 0
    | (head cities) `elem` (allCities people)  = getAverageBalance (accounts, people) (\ (_,_,city) -> city == (head cities))
    | otherwise                                = averageBalanceOfCities (accounts, people) (tail cities) 

-- task 2
data BTree = Empty | Node Int BTree BTree

t1 :: BTree                                       -- 16
t1 = Node 16 (Node 0 Empty Empty)                 -- / \
             (Node 4 (Node 1 Empty Empty)        -- 0   4
                     (Node 0 Empty Empty))       --    / \
                                              --      1   0


t2 :: BTree                                        -- 4
t2 = Node 4 (Node 0 Empty Empty)                  -- / \
            (Node 2 (Node 1 Empty Empty)         -- 0   2
                                  Empty)         --     /
                                                --      1 


countChildren :: BTree -> Int
countChildren (Node _ Empty Empty) = 0
countChildren (Node _ lt Empty) = 1
countChildren (Node _ Empty rt) = 1
countChildren (Node _ lt rt) = 2

countInteresting :: BTree -> Int
countInteresting (Node v Empty Empty) 
    | 1 == v    = 1
    | otherwise = 0
countInteresting a@(Node v lt Empty) 
    | 2^k == v = 1 + countInteresting lt 
    | otherwise = countInteresting lt 
        where 
            k = countChildren a 
countInteresting a@(Node v Empty rt)
    | 2^k == v = 1 + countInteresting rt 
    | otherwise = countInteresting rt 
        where
            k = countChildren a
countInteresting a@(Node v lt rt)
    | 2^k == v = 1 + countInteresting lt + countInteresting rt 
    | otherwise      = countInteresting lt + countInteresting rt
        where 
            k = countChildren a

