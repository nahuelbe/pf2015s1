data Tree a = Nil | Bin (Tree a) a (Tree a)


leaf :: Tree a -> Bool
leaf (Bin Nil _ Nil) = True
leaf _               = False


leaves :: Tree a -> Int
leaves Nil = 0
leaves t@(Bin l e r) | leaf t    = 1
                     | otherwise = leaves l + leaves r

internals :: Tree a -> Int
internals Nil = 0
internals t@(Bin l e r) | leaf t    = 0
                        | otherwise = 1 + internals l + internals r


{-
Queremos ver que: P(t) = leaves t <= internals t + 1

Caso base: P(Nil)
		leaves Nil <= internals Nil + 1
	leaves.1    => 0 <= internals Nil + 1
	internals.1 => 0 <= 0 + 1

Paso inductivo: P(tl) & P(tr) => P(Bin tl e tr)
		leaves (Bin tl e tr) <= internals (Bin tl e tr) + 1

	Caso 1: leaf (Bin tl e tr)
		leaves.2    => 1 <= internals (Bin tl e tr) + 1
		internals.2 => 1 <= 0 + 1

	Caso 2: not . leaf (Bin tl e tr)
		leaves.3    => leaves tl + leaves tr <= internals (Bin tl e tr) + 1
		internals.3 => leaves tl + leaves tr <= 1 + internals tl + internals tr + 1
		lema.1 + HI => True

lema.1:  a < b ^ c < d => a + b < c + d

-}



inorder :: Tree a -> [a]
inorder Nil         = []
inorder (Bin l e r) = inorder l ++ [e] ++ inorder r


mirror :: Tree a -> Tree a
mirror Nil         = Nil
mirror (Bin l e r) = Bin (mirror r) e (mirror l)


{-
Queremos ver que: P(t) = inorder t = reverse (inorder (mirror t))

Caso base: P(Nil)
		inorder Nil = reverse (inorder (mirror Nil))
	mirror.1  => inorder Nil = reverse (inorder Nil)
	inorder.1 => [] = reverse []
	reverse.1 => [] = []

Paso inductivo: P(l) ^ P(r) => P(Bin l e r)
		inorder (Bin l e r) = reverse (inorder (mirror (Bin l e r))
	mirror.2  => inorder (Bin l e r) = reverse (inorder (Bin (mirror r) e (mirror l))
	inorder.2 => inorder l ++ [e] ++ inorder r = reverse (inorder r ++ [e] ++ inorder l)
	lema      => inorder l ++ [e] ++ inorder r = reverse ([e] ++ inorder l) ++ reverse (inorder r)
	(++), reverse => inorder l ++ [e] ++ inorder r = reverse inorder l ++ [e] ++ reverse (inorder r)
	HIx2      => True

-}




{- Desafío: -}

data Poly = Cte Int | Var | Add Poly Poly | Mul Poly Poly


eval :: Poly -> Int -> Int
eval (Cte n) _ = n
eval Var x = x
eval (Add p q) x = eval p x + eval q x
eval (Mul p q) x = eval p x * eval q x


derive :: Poly -> Poly
derive (Cte n) = Cte 0
derive X = Cte 1
derive (Add p q) = Add (derive p) (derive q)
derive (Mul p q) = Add (Mul (derive p) q) (Mul p (derive q))


{-
  Queremos ver que: P(p) = eval p 0 == 0 => eval p 1 <= eval (derive p) 1
  (Para polinomios con coeficientes positivos)
-}


