

 toDigits 0 = []
 toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

 toDigitsRev 0 = []
 toDigitsRev n = [n `mod` 10] ++ toDigitsRev (n `div` 10)

 doubleSecond [] = []
 doubleSecond [x] = [x]
 doubleSecond (x:xs) = [x] ++ [(head xs) * 2] ++ (doubleSecond (tail xs))
 
 sumDigits [] = 0
 sumDigits xs = sum [ y | el <- xs, y <- (toDigits el) ]

 isValid n = sumDigs `mod` 10 == 0
     where digs = toDigitsRev n
           dsec = doubleSecond digs
           sumDigs = sumDigits dsec

  
 eval xs = foldl (\x y -> y + (10 * x)) 0 xs

 eval2 n = all (\d -> d >= 0 && d < 10) (toDigits n)

 evalRev xs = foldr (\x y -> x + (10*y)) 0 xs

 evalRevAll n = all (\d -> d >= 0 && d < 10) (toDigitsRev n)
