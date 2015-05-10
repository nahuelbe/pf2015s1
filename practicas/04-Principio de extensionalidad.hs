data Nat = Zero | Suc Nat

add :: Nat -> Nat -> Nat
add n Zero = n
add n (Suc m) = add (Suc n) m

sub :: Nat -> Nat -> Nat
sub n Zero = n
sub (Suc n) (Suc m) = sub n m

mul :: Nat -> Nat -> Nat
mul _ Zero    = Zero
mul n (Suc m) = add n (mul n m)

{- Reducir: mul (Suc (Suc Zero)) (Suc (Suc Zero))

mul.2 => add (Suc (Suc Zero)) (mul (Suc (Suc Zero)) (Suc Zero))
mul.2 => add (Suc (Suc Zero)) (add (Suc (Suc Zero)) (mul (Suc (Suc Zero)) Zero)
mul.1 => add (Suc (Suc Zero)) (add (Suc (Suc Zero)) Zero)
add.1 => add (Suc (Suc Zero)) (Suc (Suc Zero))
add.2 => add (Suc (Suc (Suc Zero))) (Suc Zero)
add.2 => add (Suc (Suc (Suc (Suc Zero)))) Zero
add.1 => Suc (Suc (Suc (Suc Zero)))

-}


{- Queremos ver que: \x -> x `mul` (Suc Zero) = x

Por principio de extensionalidad basta ver que para todo x:
        x `mul` (Suc Zero) = x
    mul.2 => x `add` (x `mul` Zero) = x
    mul.1 => x `add` Zero = x
    add.1 => x = x

-}


{- Queremos ver que:
    (\x -> (x `add` (Suc Zero)) `sub` (Suc Zero)) = id::Nat

Entonces tenemos que ver que para todo x:
        (x `add` (Suc Zero)) `sub` (Suc Zero) = id x
    add.2 => ((Suc x) `add` Zero) `sub` (Suc Zero) = id x
    add.1 => (Suc x) `sub`(Suc Zero) = id x
    sub.2 => x `sub` Zero = id x
    sub.1 => x = id x
    id.1  => x = x
-}


{- Ahora nos gustaría ver que:
    curry (uncurry f) = f

Luego para todo x e y debe cumplirse que:
        curry (uncurry f) x y = f x y
    uncurry.1 => curry (\(z, w) -> f z w) x y = f x y
    curry.1   => (\z' w' -> (\(z, w) -> f z w) (z', w')) x y = f x y
              => (\(z, w) -> f z w) (x, y) = f x y
              => f x y = f x y
-}


{- Veamos otro caso interesate:
    flip (curry f) = curry (f . swap)

Veamos, entonces, que para todo x1, x2:
        flip (curry f) x y = curry (f . swap) x y
    curry => flip (\z w -> f (z,w)) x y = (\z w -> (f . swap) (z,w)) x y
    flip  => (\w' z' -> (\z w -> f (z, w)) z' w') x y = (\z w -> (f . swap) (z,w)) x y
          => (\z w -> f (z, w)) y x = (\z w -> (f . swap) (z,w)) x y
          => f (y, x) = (\z w -> (f . swap) (z,w)) x y
          => f (y, x) = (f . swap) (x,y)
    (.)   => f (y, x) = (\z -> f (swap z)) (x,y)
          => f (y, x) = f (swap (x,y))
    swap  => f (y, x) = f (y,x)
-}

