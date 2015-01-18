
 replicate1 n a = [a | _ <- [1..n]]

 pyths1 n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

 factors n = [x| x <- [1..n], n `mod` x == 0]


 isPrf num = sum (factors num)

 perfects1 n = [x| x <- [1..n], isPerfect x]
     where isPerfect num = sum (factors num) == num

 perfects2 n = [x| x <- [1..n], isPerfect x]
     where isPerfect num = sum (init (factors num)) == num

 find k t = [v | (k', v) <- t, k == k']


 positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
      where n = length xs - 1

 positions1 x xs = find x (zip xs [0..n])
      where n = length xs - 1

 scalarproduct1 xs ys = sum [x * y | x <- xs, y <- ys]

 scalarproduct2 xs ys = product (zipWith (+) xs ys)

 scalarproduct3 xs ys = sum [ x* y| (x, y) <- xs `zip` ys]


 riffle1 xs ys = concat [[x, y] | x <- xs, y <- ys]

 riffle2 xs ys = concat [[x, y] | (x, y) <- xs `zip` ys]

-- riffle3 xs ys = [x, y | (x, y) <- xs `zip` ys]

 riffle4 xs ys = [x : [y] | x <- xs, y <- ys]
 

