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

pairs' :: [Int] -> [Int]
pairs' [] = []
pairs' (x:xs) = if even x then x:(pairs' xs) else pairs' xs

moreThan :: [[a]] -> Int -> [[a]]
moreThan [] _ = []
moreThan (xs:xss) n = if length xs > n then xs:(moreThan xss n) else moreThan xss n

ifThenElseLam = \x -> x

trueLam = \x -> \y -> x

falseLam = \x -> \y -> y

notLam = \x -> ifThenElseLam x falseLam trueLam

andLam = \x y -> ifThenElseLam x (ifThenElseLam y trueLam falseLam) falseLam

orLam = \x y -> andLam (notLam x) (notLam y)

iffLam = \p q -> orLam (notLam p) q

xorLam = \x y -> orLam (andLam (notLam x) y) (andLam x (notLam y))

