
 merge [] ys = ys
 merge xs [] = xs
 merge (x : xs) (y : ys)
   = if x <= y then x : merge xs (y: ys) else y : merge (x : xs) ys

 halve xs = splitAt (length xs `div` 2) xs

 msort [] = []
 msort [x] = [x]
 msort xs = merge (msort ys) (msort zs)
   where (ys, zs) = halve xs

