import Prelude() -- Ocultamos las definiciones base del Prelude de Haskell
                 -- porque vamos a definir algunas nosotros

-- En Haskell escribimos programas usando funciones y patrones
-- El keyword data nos permite crear tipos algebráicos, entre ellos tipos enumerados como los días de la semana
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

-- Podemos escribir funciones para los días usando pattern matching sobre los constructores
next :: Day -> Day
next Mon = Tue
next Tue = Wed
next Wed = Thu
next Fri = Sat
next Sat = Sun
next Sun = Mon

-- Otro tipo enumerado es Bool
data Bool = True | False

weekend :: Day -> Bool
weekend Sat = True
weekend Sun = True
weekend day = False

-- También podemos escribir funciones para los Bool usando pattern matching sobre los constructores
not :: Bool -> Bool
not True  = False
not False = True

and :: Bool -> Bool -> Bool
and False _ = False
and True  y = y

or :: Bool -> Bool -> Bool
or True  _ = True
or False y = y

-- No siempre es necesario saber el tipo de los parámetros, podemos escribir funciones polimórficas
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  thenBranch elseBranch = thenBranch
ifThenElse False thenBranch elseBranch = elseBranch

-- También podemos crear tipos compuestos como los pares o el tipo maybe
data Pair a b = Pair a b

first :: Pair a b -> a
first (Pair x _) = x

second :: Pair a b -> b
second (Pair _ y) = y

data Maybe a = Nothing | Just a

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

fromJust :: Maybe a -> a
fromJust (Just x) = x


-- Finalmente podemos crear tipos recursivos como los naturales
data Nat = Zero | Suc Nat

isZero :: Nat -> Bool
isZero Zero = True
isZero _    = False

add :: Nat -> Nat -> Nat
add Zero    m = m
add (Suc n) m = Suc (add n m)

sub :: Nat -> Nat -> Maybe Nat -- Usaremos Maybe para evitar casos de error
sub Zero    m       = Nothing
sub n       Zero    = Just n
sub (Suc n) (Suc m) = sub n m

even :: Nat -> Bool
even Zero    = True
even (Suc n) = odd n

odd :: Nat -> Bool
odd Zero     = True
odd (Suc n)  = even n

-- Veamos las operaciones de igualdad de los Bool y los Nat
equalBool :: Bool -> Bool -> Bool
equalBool True  True  = True
equalBool False False = True
equalBool _     _     = False

equalNat :: Nat -> Nat -> Bool
equalNat Zero    Zero    = True
equalNat (Suc n) (Suc m) = equalNat n m
equalNat _       _       = False

-- Usando el (==) podríamos escribir un operador (/=) polimórfico. El problema es cómo forzar a que los
-- parámetros tengan un operador (==). El keyword class nos permite definir datos con la misma interfaz
class Eq a where
  (==)  :: a -> a -> Bool

-- El keyword instance nos permite vincular cada operación de una clase con una operación del tipo
instance Eq Bool where
  (==) = equalBool

instance Eq Nat where
  (==) = equalNat

(/=) :: Eq a => a -> a -> Bool
(/=) x y = not (x == y)


-- ¿Cómo podemos construir una lista?
data List a = Nil | Cons a (List a)

null :: List a -> Bool
null Nil = True
null _   = False

length :: List a -> Nat
length Nil         = Zero
length (Cons x xs) = Suc (length xs)

elem :: Eq a => a -> List a -> Bool
elem y Nil         = False
elem y (Cons x xs) = (y == x) `or` elem y xs

