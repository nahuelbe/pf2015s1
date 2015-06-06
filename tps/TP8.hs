data TipTree a = Tip a | Join (TipTree a) (TipTree a) deriving (Show)

foldTip :: (a -> b) -> (b -> b -> b) -> TipTree a -> b
foldTip ft fj (Tip x) = ft x
foldTip ft fj (Join l r) = fj (foldTip ft fj l) (foldTip ft fj r)

heightTip' :: TipTree a -> Int
heightTip' = foldTip (\x -> 0) (\x y -> 1 + max x y)

leaves' :: TipTree a -> Int
leaves' = foldTip (\x -> 1) (\rl rr -> rl + rr)