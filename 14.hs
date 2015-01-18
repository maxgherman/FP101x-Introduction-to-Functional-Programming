  
-- import Prelude hiding ((!!))

-- m ^ 0 = 1
-- m ^ n = m*m ^ (n -1)

 and1 [] = True
 and1 (b : bs) = b && and1 bs

 and2 [] = True
 and2 (b:bs)
   | b = and bs
   | otherwise = False

 and3 [] = False
 and3 (b:bs) = b && and3 bs

 and4 [] = True
 and4 (b:bs) = b || and bs

 and5 [] = True
 and5 (b:bs)
   | b == False = False
   | otherwise = and5 bs

 and6 [] = True
 and6 (b:bs) = and6 bs && b


 concat1 [] = []
 concat1 (xs : xss) = xs ++ concat1 xss

 concat2 [] = []
 concat2 (xs : xss) = xs : concat2 xss

 replicate1 0 _ = []
 replicate1 n x = x ++ replicate1 (n-1) x

 --(x:_) !! 0 = x
 --(_:xs) !! n = xs !! (n-1)

 elem1 _ [] = False
 elem1 x (y:ys)
   | x == y = True
   | otherwise = elem1 x ys









 
