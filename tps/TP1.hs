seven :: Integer -> Integer
seven _ =  7

sign :: Integer -> Integer
sign x | x > 0 = 1
	   | x < 0 = -1
	   | otherwise = 0

sign' ::  Integer -> Integer
sign' x = if x > 0 
		  then 1
		  else if x < 0 
		  	   then -1
		  	   else 0

absolute :: Integer -> Integer
absolute x | sign x < 0 = -x
		   | otherwise = x

absolute' :: Integer -> Integer
absolute' x = if x < 0 
			  then -x
			  else x

and' :: Bool -> Bool -> Bool
and' False _ = False
and' _ False = False
and' _ _ = True

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

not' :: Bool -> Bool
not' True = False
not' _ = True

xor' :: Bool -> Bool -> Bool
xor' False True = True
xor' True False = True
xor' _ _ = False

dividesTo :: Integer -> Integer -> Bool
dividesTo x y | mod y x < y = True
			  | otherwise = False

isMultiple :: Integer -> Integer -> Bool
isMultiple x y = if x < y then mod y x == 0
				 else mod x y == 0

isDivisor :: Integer -> Integer -> Bool
isDivisor x y = mod y x == 0 

isCommonDivisor :: Integer -> (Integer, Integer) -> Bool
isCommonDivisor x (y1, y2) = isDivisor x y1 && isDivisor x y2

isCommonMult :: Integer -> (Integer, Integer) -> Bool
isCommonMult x (y1, y2) = isMultiple x y1 && isMultiple x y2

swap :: (Integer, Integer) -> (Integer, Integer)
swap (x,y) = (y,x)


-- Ejercicio 2

-- a)

f :: a -> (a,a)
f x = (x,x)

f' :: a -> b -> (a,a)
f' x = let e y = (x,x)
	   in e

-- b)

greaterThan :: (Integer,Integer) -> Bool
greaterThan (x,y) | x > y = True
				  | otherwise = False

sort3 :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
sort3 t = addFirstTo3Tuple (getMax t) (sort2 (removeMax t))

getMax :: (Integer, Integer, Integer) -> Integer
getMax (x,y,z) = if x > y && x > z
				 then x
				 else if y > x && y > z
				 	  then y
				 	  else z

removeMax :: (Integer, Integer, Integer) -> (Integer, Integer)
removeMax (x,y,z) = if x > y && x > z
					then (y,z)
					else if y > x && y > z
						 then (x,z)
						 else (x,y)

sort2 :: (Integer, Integer) -> (Integer, Integer)
sort2 (x,y) = if x > y
			  then (x,y)
			  else (y,x)

addFirstTo3Tuple :: Integer -> (Integer, Integer) -> (Integer, Integer, Integer)
addFirstTo3Tuple x (y,z) = (x,y,z)