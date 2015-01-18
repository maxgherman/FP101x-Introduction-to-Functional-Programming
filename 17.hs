
 p x = x `div`2 == 2
 p1 x = x > 0
 p2 x = x < 0

 func1 f p xs = [f x | x <- xs, p x]

 func2 f p xs = map f (filter p xs)

 all1 p xs = and (map p xs)

 all3 p = and . map p

 all4 p = not . any (not . p)

 all6 p xs = foldl (&&) True (map p xs)

 all7 p xs = foldl (&&) False (map p xs)

 all8 p = foldl (&&) True . map p


 any2 p = or . map p

 any3 p xs = length (filter p xs) > 0

 any4 p = not . null . dropWhile (not . p)

 any6 p xs = not (all (\ x -> not (p x)) xs)

 any7 p xs = foldr (\x acc -> (p x) || acc) False xs


 takeWhile1 _ [] = []
 takeWhile1 p (x:xs) 
   | p x = x : takeWhile1 p xs
   | otherwise = takeWhile1 p xs

 takeWhile2 p = foldl (\ acc x -> if p x then x : acc else acc) []

 takeWhile3 _ [] = []
 takeWhile3 p (x:xs)
   | p x = x : takeWhile3 p xs
   | otherwise = []


 dropWhile1 _ [] = []
 dropWhile1 p (x:xs)
  | p x = dropWhile1 p xs
  | otherwise = x : xs

 dropWhile2 _ [] = []
 dropWhile2 p (x:xs)
  | p x = dropWhile2 p xs
  | otherwise = xs

 dropWhile3 p = foldr(\ x acc -> if p x then acc else x : acc) []

 dropWhile4 p = foldl add []
  where add [] x = if p x then [] else [x]
        add acc x = x : acc


 map1 f = foldr (\ x xs -> xs ++ [f x]) []
 map3 f = foldl (\ xs x -> f x : xs) []
 map4 f = foldl (\ xs x -> xs ++ [f x]) []


 filter2 p = foldr (\ x xs -> if p x then x : xs else xs) []

 compose = foldr (.) id
 --sumsqreven = compose [sum, map (^2), filter even]

 f1 (x, y) = x * y
 curry1 f = \ x y -> f x y
 curry2 f = \ x y -> f
 curry3 f = \ x y -> f (x, y)
 curry4 f = \ (x, y) -> f x y

 f2 x y = x * y
 uncurry1 f = \ (x, y) -> f x y

 unfold1 p h t x
  | p x = []
  | otherwise = h x : unfold1 p h t (t x)

 chop8 [] = []
 chop8 bits = take 8 bits :chop8 (drop 8 bits)

 chop81 = unfold1 null (take 8) (drop 8)

 map5 f = unfold1 null (f . head) tail

 iterate1 f = unfold1 (const False) id f


 ff x = x + 1
 pp x = x > 0








