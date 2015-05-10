5. 

a) \x y -> x (x y)

b) \x y z -> x z y

c) \x -> x + 1

6. 

a) fix :: (b -> a -> b) -> a -> b

b) fork :: (a -> b , a -> c) -> a -> (b,c)

c) apply :: (a -> b) -> a -> b

d) curry :: ((a,b) -> c) -> a -> b -> c