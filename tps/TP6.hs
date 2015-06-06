data TipTree a = Tip a | Join (TipTree a) (TipTree a) deriving (Show)

heightTip :: TipTree a -> Int
heightTip (Tip x) = 0
heightTip (Join l r) = 1 + (max (heightTip l) (heightTip r))

leaves :: TipTree a -> Int
leaves (Tip x) = 1
leaves (Join l r) = (leaves l) + (leaves r)

nodes :: TipTree a -> Int
nodes (Tip x) = 0
nodes (Join l r) = 1 + (nodes l) + (nodes r)

walkover :: TipTree a -> [TipTree a]
walkover (Tip x) = [Tip x]
walkover (Join l r) = (walkover l) ++ (walkover r)

mirrorTip :: TipTree a -> TipTree a
mirrorTip (Tip x) = Tip x
mirrorTip (Join l r) = Join (mirrorTip r) (mirrorTip l)

mapTip :: (a -> b) -> TipTree a -> TipTree b
mapTip f (Tip x) = Tip (f x)
mapTip f (Join l r) = Join (mapTip f l) (mapTip f r)