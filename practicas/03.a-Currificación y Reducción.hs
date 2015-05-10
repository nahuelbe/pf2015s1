-- Sabemos que: (1) las funciones son valores y
--              (2) toda función tiene un único parámetro
-- Entonces definamos flip :: (a -> b -> c) -> (b -> a -> c)
-- Definamos uncurry :: (a -> b -> c) -> ((a, b) -> c)
-- Definamos curry :: ((a, b) -> c) -> (a -> b -> c)


-- Podmos definir un conjunto con su función característica
type Set a = a -> Bool


-- Como las funciones son valores podemos definir operaciones básicas
belongs :: a -> Set a -> Bool
belongs e q = q e

singleton :: Eq a => a -> Set a
singleton e = (e==)


-- Podemos definir, también, conjuntos con funciones
empty = const False

universal = const True


-- Podemos operar sobre conjuntos como lo hacemos sobre funciones
complement :: Set a -> Set a
complement q = not . q

union :: Set a -> Set a -> Set a
union q s = \e -> q e || s e

intersection :: Set a -> Set a -> Set a
intersection q s = \e -> q e && s e

cartesianProduct :: Set a -> Set b -> Set (a, b)
cartesianProduct q s = \(e, o) -> q e && s o

difference :: Set a -> Set a -> Set a
difference q s = intersection q (complement s)

symmetricDifference :: Set a -> Set a -> Set a
symmetricDifference q s = union (difference q s) (difference s q)

-- Creemos un conjunto a partir de una lista
listToSet :: Eq a => [a] -> Set a
listToSet = flip elem

{- ¿Cómo se puede obtener el conjuto de todos los pares donde
   el segundo elemento es mayor que el primero? -}
gtPairs :: Ord a => Set (a,a)
gtPairs = uncurry (<)

{- ¿Cómo obtenemos a partir de un conjunto de pares (a,b) el subconjunto de los
   b(s) asociados con un a dado? -}
image :: Set (a,b) -> a -> Set b
image = curry

-- ¿Cuál es el cardinal del siguiente conjuto? ¿Cuál es el cardinal de su intersección?
evenOddPairs = cartesianProduct even odd

-- ¿Cómo definimos la igualdad y el cardinal sobre esta representación de conjuntos?
{- Respuesta: Esto no es posible en general, dado que es un problema equivalente al
              Halting Problem. -} 

{- ¿Qué sucede cuando un número mayor a 0 no pertenece al conjunto de collatz?
   ¿Cuántos elementos tiene este conjunto? -}
collatz :: Set Int
collatz n | n == 1 = True
          | even n = collatz (n `div` 2)
          | odd n  = collatz (3*n + 1)

{- DESAFÍO: Definir las operaciones de complemento, union e intersección para
   FuzzySets (conjuntos donde no se sabe con seguridad si un elemento pertenece
   o no al conjunto sino que se tiene un valor de certeza, es decir, un real entre
   0 y 1). Considerando el siguiente tipo: -}

type FuzzySet a = a -> Float

