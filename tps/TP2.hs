{- 
1. 

a) True, False
b) (1,2), (2,1)
c) digitToInt, ord
d) 

2. 

3. seven :: Int -> Int
sign :: Int -> Int
absolute :: Int -> Int
and' or' xor' :: Bool -> Bool -> Bool
dividesTo :: Int -> Int -> Bool
isMultiple :: Int -> Int -> Bool
isCommonDivisor :: Int -> (Int,Int) -> Bool
isCommonMult :: Int -> (Int,Int) -> Bool
swap :: (a,b) -> (b,a)

4. a) first :: (a,b) -> a
	  first (x,y) = x

b) second :: (a,b) -> b
second (x,y) = y

c) const :: a -> b -> a
const x y = x

d) compose ::  (a -> b) -> (c -> a) -> b
compose f g = (\x -> f (g x))

e) apply :: (a -> b) -> a -> b
apply f x = f x

f) subst :: (a -> b -> c) -> (c -> b) -> c
subst f g x = f x (g x)

g) pairFunc :: ((b -> a),(a -> b)) -> a -> b -> (a,b)
pairFunc (f1,f2) x y = (f1 (f2 x), f2 (f1 y))

6. 

a) if (seven ’a’ < 1) then 5 else power4 2

Error de tipo seven sólo recibe parámetros de tipo Integer

b) if False then True

Error de sintaxis falta especificar la rama del else.

c) a := 4

:= no es un constructor definido en hugs

d) (1 < x && x < 100) || x == True || x == False

primero que todo daría error de tipos porque x
no puede ser un integer y un booleano al mismo 
tiempo

x no está definido

e) False

f) (1 < x < 2)

está bien formada pero x no está definido

11. 

a) \x y z -> if x<=y && x <= z then x else if y <= x && y <= z then y else z

b) \x y -> y

c) \x y -> y && x 

12.

a) iff :: Bool -> Bool -> Bool
   iff x y = if x then not y
   			 else y

b) alpha :: a -> b -> b
   alpha x y = y

-}

data ColorPrimario = Rojo | Azul | Amarillo deriving (Show)
data ColorSecundario = Compuesto ColorPrimario ColorPrimario deriving (Show)

mezclar :: ColorPrimario -> ColorPrimario -> ColorSecundario
mezclar Rojo Rojo = error "No se puede mezclar el mismo color"
mezclar Amarillo Amarillo = error "No se puede mezclar el mismo color"
mezclar Azul Azul = error "No se puede mezclar el mismo color"
mezclar c1 c2 = Compuesto c1 c2



