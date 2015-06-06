-- Recordemos

-- foldr :: (a -> b -> b) -> [a] -> b -> [b]

-- map :: (a -> b) -> [a] -> [b]
-- filter :: (a -> Bool) -> [a] -> [a]
-- concatMap :: (a -> [b]) -> [a] -> [b]

-- all/any :: (a -> Bool) -> [a] -> Bool
-- and/or :: [Bool] -> Bool
-- sum/product :: Num a => [a] -> a

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
-- zipWith _ _ _ = []

-- zip :: [a] -> [b] -> [(a,b)]
-- zip = zipWith (,)


index :: [a] -> [(Int,a)]
index = zip [1..]


{-
Queremos ver que: sum (xs ++ ys) = sum (zipWith (+) xs ys)

Caso base (xs = []):

        sum ([] ++ ys) = sum (zipWith (+) [] ys)
    zipWith.2 => sum ([] ++ ys) = sum []
    (++).1 => sum ys = sum []
    
        ¿Qué pasó?
		Ver demostración al final
-}


type Matrix a = [[a]]
-- Invariante: Todas las listas internes tienen la misma longitud (repok)

repok :: Matrix a -> Bool
repok (xs:xss) = all (length xs ==) (map length xss)


id_3x3 = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]::Matrix Int
ma_3x3 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]::Matrix Int 


mGet :: Matrix a -> Int -> Int -> a
mGet m i j = (m !! i) !! j


mulScalar :: Int -> Matrix Int -> Matrix Int
mulScalar n x = map (map (n*)) x


mAdd :: Matrix Int -> Matrix Int -> Matrix Int
mAdd x y = zipWith (zipWith (+)) x y


mAll :: (a -> Bool) -> Matrix a -> Bool
mAll p x = and (map (all p) x)

