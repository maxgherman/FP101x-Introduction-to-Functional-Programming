
 safetail1 xs = if null xs then [] else tail xs         --X

 safetail2 [] = []
 safetail2 (_:xs) = xs                                  --X

 safetail3 (_ : xs)
   | null xs = []
   | otherwise = tail xs                                

 safetail4 xs
   | null xs = []
   | otherwise = tail xs				--X

 --safetail5 xs = tail xs
 --safetail5 [] = []

 safetail6 [] = []
 safetail6 xs = tail xs					--X

 safetail7 [x] = [x]
 safetail7 (_ : xs) = xs				

 safetail8
   = \ xs ->
       case xs of
	    [] -> []
	    (_ : xs) -> xs				--X
