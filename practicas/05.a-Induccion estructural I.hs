data Nat = Zero | Suc Nat deriving Show

even :: Nat -> Bool
even Zero = True
even (Suc n) = odd n

odd :: Nat -> Bool
odd Zero = False
odd (Suc n) = even n

{-
Queremos ver que:
    (\x -> even x || odd x) = const True

Tenemos que ver que para todo x sucede que:
        even x || odd x = const True x
    const.1 => even x || odd x = True

Necesitamos distinguir en casos para continuar:

- Caso base ( x = Zero ):

        even Zero || odd Zero = True
    even.1 => True || odd Zero = True
    (||).1 => True = True


- Paso inductivo:
Supongamos que  even n || odd n = True  es cierto e intentemos ver que también lo es para n+1.

        even (Suc n) || odd (Suc n) = True
    even.2 => odd n || odd (Suc n) = True
    odd.2  => odd n || even (Suc n) = True
    (||).conmutativity => even n || odd n = True -- que es nuestra hipótesis inductiva


Idem para: (\x -> even x && odd x) = const False

-}


-- Propiedades para listas

{-
P(xs) = length (xs ++ ys) = length xs + length ys

- Caso base: P([]):
        length ([] + ys) = length [] + length ys
    length.1 => length ([] ++ ys) = 0 + length ys
    (++).1   => length ys = length ys

- Paso inuctivo: P(xs) => P(x:xs)
        length ((x:xs) ++ ys) = length (x:xs) + length ys
    (++).2   => length (x : (xs ++ ys)) = length (x:xs) + length ys
    length.2 => 1 + length (xs ++ ys) = 1 + length xs + length ys
    arit     => length (xs ++ ys) = length xs + length ys -- our hipothesis

-}


{-
P(xs) = reverse (xs ++ ys) = reverse ys ++ reverse xs

- Caso base: P([]):
        reverse ([] ++ ys) = reverse ys ++ reverse []
    (++).1     => reverse ys = reverse ys ++ reverse []
    reverse.1  => reverse ys = reverse ys ++ []
    lema(++[]) => reverse ys = reverse ys

- Paso inductivo: P(xs) => P(x:xs)
        reverse ((x:xs) ++ ys) = reverse ys ++ reverse (x:xs)
    (++).2    => reverse (x:(xs ++ ys)) = reverse ys ++ reverse (x:xs)
    revrese.2 => reverse (xs ++ ys) ++ [x] = reverse ys ++ reverse xs ++ [x]
    HI        => True

-}


{-
P(xs) = reverse (reverse xs) = xs

- Caso base: P([]):
        reverse (reverse []) = []
    reverse.1 => reverse [] = []
    reverse.1 => reverse [] = []

- Paso inductivo: P(xs) => P(x:xs)
        reverse (reverse (x:xs)) = x:xs
    reverse.2 => reverse (reverse xs ++ [x]) = x:xs
    ej ant    => reverse [x] ++ reverse (reverse xs) = x:xs
    reverse.2 => reverse [] ++ [x] ++ reverse (reverse xs) = x:xs
    reverse.1 => [] ++ [x] ++ reverse (reverse xs) = x:xs
    (++).1    => [x] ++ reverse (reverse xs) = x:xs
    (++).2    => x:(reverse (reverse xs)) = x:xs
    HI        => True

-}

{- Dado gauss = sum id 1, queremos ver que:
        gauss = \n -> n * (n+1) / 2
   Se puede hacer por inducción, pero es largo y cuentoso.
   Gauss lo demostró más fácilmente.
-}


{- DESAFÍO: Queremos ver que para todo xs, i:
     0 <= i < length xs => (reverse xs) !! i = xs !! (length xs - i - 1)
   AYUDA: Separe en casos, identifique lemas auxiliares útiles y demuestrelos aparte.
-}

