-- Funciones simples y el uso del lambda para construir funciones anónimas

id x = x     -- id es la función que toma x y devuelve x

id' = \x -> x -- id' es el valor \x -> x (que es una función que toma x y devuelve x)

-- Como las funciones son valores podemos definir funciones que operen con funciones

-- A partir de los siguientes tipos escribir funciones que los satisfagan
apply :: (a -> b) -> a -> b
apply f x = f x

-- Podemos escribir apply como un operador
($) :: (a -> b) -> a -> b
($) f x = f x

flipApply :: a -> (a -> b) -> b
flipApply x f = f x

maybeApply :: (a -> Maybe b) -> Maybe a -> Maybe b
maybeApply f Nothing  = Nothing
maybeApply f (Just x) = f x

{- También podemos definir funciones sin parámetros, ya que las expresiones a
   la derecha del igual denotan funciones (incluso los constructores) -}
liftMaybe :: x -> Maybe x
liftMaybe = Just

inc :: Nat -> Nat
inc = Suc

-- Podemos definir funciones que retornan funciones como resultado
const :: a -> (b -> a)
const x = \y -> x

-- A partir del código de (.) dar su tipo
(.) :: (a -> b) -> (b -> c) -> (a -> c)
(.) f g = (\x -> g (f x))

-- Definir twice usando (.)
twice :: (a -> a) -> (a -> a)
twice f = f . f

-- Definir inc2 usando twice
inc2 :: Nat -> Nat
inc2 = twice inc

-- Definir pred3 usando maybeApply
pred :: Nat -> Maybe Nat
pred n = sub n (Suc Zero)

pred3 :: Nat -> Maybe Nat
pred3 n = let p1 = pred n
              p2 = maybeApply pred p1
              p3 = maybeApply pred p2
           in p3
 
