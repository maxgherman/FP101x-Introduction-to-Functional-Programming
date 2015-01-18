
 data Expr = Add Expr Expr | Val Int

 test1 = Add (Val 1) (Val 2)

 
 data Tree = Leaf Int | Node Tree Tree

 test2 = Node (Leaf 1) (Leaf 2)

 data Maybe1 a = Nothing1 | Just1 a

 --instance Monad Maybe1 where
-- 	return x = Just1 x
 --	Nothing1 >>= f = Just1 (f (Nothing1))

 instance Monad Maybe1 where
	return x = Just1 x
	Nothing1 >>= _ = Nothing1
	(Just1 x) >>= f = f x

 --instance Monad Maybe1 where
--	return x = Just1 x
--	Nothing1 >>= _ = Nothing1
--	(Just1 x) >>= f = Just (f x)

 --instance Monad Maybe1 where
 --	return x = Just1 x
 --	Nothing1 >>= f = f Nothing1
 --	(Just1 x) >>= f = f x


