sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs 

any' :: [Bool] -> Bool
any' [] = False
any' (x:xs) = x || any' xs

all' :: [Bool] -> Bool
all' [] = True
all' (x:xs) = x && all' xs

remainders' :: [Int] -> Int -> [Int]
remainders' [] _ = []
remainders' (x:xs) n = (mod x n) : (remainders' xs n)

squares' :: [Int] -> [Int]
squares' [] = []
squares' (x:xs) = x*x : (squares' xs)

lengths' :: [[a]] -> [Int]
lengths' [] = []
lengths' (x:xs) = length x : (lengths' xs)

order' :: [(Int,Int)] -> [(Int,Int)]
order' [] = []
order' (x:xs) = if fst x < (snd x * 3) then x : (order' xs) else order' xs