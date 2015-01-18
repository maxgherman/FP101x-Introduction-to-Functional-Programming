
 divides x y = (x `mod` y) == 0

 divisors x = [d | d <- [1..x], x `divides` d]
