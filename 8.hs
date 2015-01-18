
 mul1 x y z = \ x -> (\y -> (\z -> x * y * z))

 --mul2 = \ x -> (x * \ y -> (y * \ z -> z))

 mul3 = \ x -> (\ y -> (\z -> x * y * z))	--X
