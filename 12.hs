
 import Data.Char

 let2int1 c = ord c - ord 'a'
 
 int2let1 n = chr (ord 'a' + n)
 
 shift1 n c
   | isLower c = int2let1 ((let2int1 c + n) `mod` 26)
   | otherwise = c

 encode1 n xs = [shift1 n x | x <- xs]



 let2intUp c = ord c - ord 'A'
 
 int2letUp n = chr (ord 'A' + n)
 
 shift n c
   | isLower c = int2let1 ((let2int1 c + n) `mod` 26)
   | isUpper c = int2letUp ((let2intUp c + n) `mod` 26)
   | otherwise = c


 encode n xs = [shift n x | x <- xs]

