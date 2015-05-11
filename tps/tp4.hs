nextDiv :: Int -> Int -> Int
nextDiv x y = if mod y (x+1) == 0 
			  then x+1
			  else nextDiv (x+1) y

sumDivs :: Int -> Int
sumDivs x = sum (getDivs 0 x)

getDivs :: Int -> Int -> [Int]
getDivs x y = if x == y
			  then []
			  else (nextDiv x y) : (getDivs (nextDiv x y) y)

power :: Int -> Int -> Int
power _ 0 = 1
power x y = x * power x (y-1)

dividesTo :: Int -> Int -> Bool
dividesTo x y = if x >= y
			 	then y == 0 || y == x
			 	else dividesTo x (y-x)

sum' :: (Int -> Int) -> Int -> Int -> Int
sum' _ _ 0 = 0
sum' f i j = (f i) + sum' f (i+1) (j-1)

prime :: Int -> Bool
prime x = isPrime x 2

isPrime :: Int -> Int -> Bool
isPrime 0 _ = True
isPrime 1 _ = True
isPrime x y = if x == y
			  then True
			  else not (dividesTo y x) && isPrime x (y+1)

phi :: Int -> Int
phi x = phiN x 0

phiN :: Int -> Int -> Int
phiN 0 y = if prime y
		   then y
		   else phiN 0 (y+1)
phiN x y = if prime y
		   then phiN (x-1) (y+1)
		   else phiN x (y+1)