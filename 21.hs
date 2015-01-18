
 data Tree = Leaf Integer
	   | Node Tree Tree

 treeb1 = Leaf 1
 treeb2 = Node (Leaf 1) (Leaf 3)
 treeb3 = Node (Node (Leaf 100) (Leaf 3)) (Node (Leaf 5) (Leaf 7))
 
 treenb1 = Node (Leaf 1) (Node ( Node  (Leaf 2) (Leaf 4)) (Leaf 3))
 treenb2 = Node (Node (Leaf 2) ( Node (Leaf 3) (Leaf 7)  )) (Leaf 1)


 leaves1 (Leaf x) = x
 leaves1 (Node l r) = leaves1 l + leaves1 r
 balanced1 (Leaf _) = True
 balanced1 (Node l r)
	= abs (leaves1 l - leaves1 r) <= 1 || balanced1 l || balanced1 r

 --leaves2 (Leaf _) = True
 --leaves2 (Node l r) = leaves2 l + leaves2 r
 --balanced2 (Leaf _) = True
 --balanced2 (Node l r)
 --	= abs (leaves2 l - leaves2 r) <= 1

 --leaves3 (Leaf _) = True
 --leaves3 (Node l r) = leaves3 l + leaves3 r
 --balanced3 (Leaf _) = True
 --balanced3 (Node l r)
 --	= abs (leaves3 l + leaves3 r) <= 1

 leaves4 (Leaf _) = 1
 leaves4 (Node l r) = leaves4 l + leaves4 r
 balanced4 (Leaf _) = True
 balanced4 (Node l r)
	= abs (leaves4 l - leaves4 r) <= 1 && balanced4 l && balanced4 r
 


 halve1 xs = splitAt (length xs `div` 2) xs
 balance1 [x] = Leaf x
 balance1 xs = Node (balance1 ys) (balance1 zs)
	where (ys, zs) = halve1 xs


 --halve2 xs = splitAt (length xs / 2) xs
 --balance2 [x] = Leaf x
 --balance2 xs = Node (balance2 ys) (balance2 zs)
 --	where (ys, zs) = halve2 xs

 --halve3 xs = splitAt (length xs `div` 2) xs
 --balance3 [x] = Leaf x
 --balance3 xs = Node ys zs
 -- 	where (ys, zs) = balance3 (halve3 xs)

 --halve4 xs = splitAt (length xs `div` 2) xs
 --balance4 x = Leaf x
 --balance4 xs = Node (balance4 ys) (balance4 zs)
 --	where (ys, zs) = halve4 xs


