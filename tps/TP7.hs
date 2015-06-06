sum' = foldr (\x r -> x + r) 0

any' = foldr (\x r -> x || r) False

all' = foldr (\x r -> x && r) True

remainders' n = foldr (\x r -> (mod x n):r) []

squares' = foldr (\x r -> (x*x):r) []

lengths' = foldr (\x r -> (length x):r) []

order' = foldr (\x r -> if fst x < (snd x * 3) then x:r else r) []

pairs' = foldr (\x r -> if even x then x:r else r) []

