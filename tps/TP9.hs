1)

class Package f where

handle :: (a -> b) -> f a -> f b

instance Package Maybe where

	handle f Nothing = Nothingst
	handle f (Just x) = Just (f x)

instance Package [] where 
	handle = map

instance Package ( (->) c ) where
	handle f g = \c -> f (g c)

	otra versión

	handle = (.)

2) 

link f g = \x -> f (g x) x

pack = const

3)

a) 

handle' :: Box m => (a -> b) -> m a -> m b
handle' f = link (pack . f)

mapM :: Monad m => (a -> m b) -> [a] -> m[b]
mapM f xs =  sequence (map f xs)

sequence [] = return []
sequence (x:xs) = x >>= \y -> 
					sequence xs >>= \ys -> 
				  return (y:ys)

succesion = pack (concatMap (\x -> link x (\e -> [e])))

4) 

instance Appendable c  => Monad ((,) c ) where

	(x,y) >>= f = let (x', y') = f y
				  in (append x x' , y')
	return y = (empty, y)

instance Monad (ST s) where
	(ST g) >>= f = ST $ \s -> let {(s', x) = gs; ST h = f x} in h s'


7)

type Coord = (Int, Int)
type Board = Coord -> Bool

guard :: Bool -> [()]
guard True = return ()
guard False = mzero

nqueens :: Int -> [Board]
nqueens n = nqueens' (const False) n n

nqueens' :: Board -> Int -> Int -> [Board]
nqueens' b n k = if k == 0 
				 then return b
				 else do 
				 	c <- [(i,j) | i <- [1..n], j <- [1..n]]
				 	guard (not $ threatened b n c)
				 	nqueens' (addQueen b c) n (k-1)


