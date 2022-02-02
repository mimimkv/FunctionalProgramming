import Data.Char (ord)
main :: IO()
main = do
    print (encode "Haskell") -- -> "Haskell"
    print (encode "aaabccdefff") -- -> "3abccde3f"
    print (encode "aaaaaaaaaaaabbb") -- -> "12a3b"
    print (encode "aabbb") -- -> "aa3b"
    print (decode "4aw3ee")
    print (decode "12a3b") -- -> "aaaaaaaaaaaabbb"
    print (decode "a3b") -- -> "abbb"
    print (decode "aa3b") -- -> "aabbb"


-- task 1
count :: String -> Int 
count xs = helper xs 1
    where 
        helper lst rep
            | null lst || null (tail lst) = rep
            | head lst == head (tail lst) = helper (tail lst) (1 + rep)
            | otherwise                   = rep


encode :: String -> String
encode xs
    | null xs = []
    | count xs > 2 = show (count xs) ++ head xs : encode (drop (count xs) xs)
    | otherwise   = head xs : encode (tail xs)





--task 2
isDigit :: Char -> Bool
isDigit ch = ('0' <= ch) && (ch <= '9')


toInt :: Char -> Int
toInt symbol = ord symbol - ord '0'




countRepetition :: String -> Int 
countRepetition xs = helper 0 xs
    where 
        helper result lst
            | null lst           = result
            | isDigit (head lst) = helper (result * 10 + toInt (head lst)) (tail lst)
            | otherwise         = result

replication :: Int -> Char -> [Char]
replication n symbol = helper 1
    where
        helper i = 
            if i > n
            then []
            else symbol : helper (i + 1)

countDigits :: Int -> Int 
countDigits n =
    if n < 10
    then 1
    else 1 + countDigits (div n 10)


decode :: String -> String
decode xs
    | null xs = []
    | countRepetition xs > 0 = replication (countRepetition xs) (head (drop (countDigits (countRepetition xs)) xs)) ++ decode (drop (countDigits (countRepetition xs) + 1) xs)
    | otherwise             = head xs : decode (tail xs)


