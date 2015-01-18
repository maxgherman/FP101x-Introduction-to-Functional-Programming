
 putStr' [] = return ()
 putStr' (x:xs) = putChar x >> putStr' xs

 putStrLn1 [] = putChar '\n'
 putStrLn1 xs = putStr' xs >> putStrLn1 ""

 putStrLn2 [] = putChar '\n'
 putStrLn2 xs = putStr' xs >> putChar '\n'

 putStrLn5 [] = putChar '\n'
 putStrLn5 xs = putStr' xs >> putStr' "\n"

 getLine1 = get1 ""
 
 get1 :: String -> IO String
 get1 xs
   = do x <- getChar
	case x of
	     '\n' -> return xs
	     _ -> get1 (x : xs)

 interact' f
   = do input <- getLine1
        putStrLn1 (f input)

 m1 =
   putStrLn "monad1" >> 
   return ("monad1")

 m2 =
   putStrLn "monad2" >> 
   return ("monad2")

 exec
   = do m1
	output <- m2
        putStrLn(output)

 sequence2 [] = return ()
 sequence2 (m : ms) = (foldl (>>) m ms) >> return ()  -- x

 sequence3 ms = foldl (>>) (return ()) ms

 sequence4 [] = return ()
 sequence4 (m : ms) = m >> sequence4 ms               -- x

 sequence5 [] = return ()
 sequence5 (m : ms) = m >>= \_ -> sequence5 ms        -- x

 sequence7 ms = foldr (>>) (return ()) ms	      -- x

 sequence8 ms = foldr (>>) (return []) ms

 
 sq1 [] = return []
 sq1 (m : ms)
  = m >>=
      \ a ->
	do as <- sq1 ms
	   return (a : as)

 --sq2 ms = foldr func (return ()) ms
--	where
--	      func :: (Monad m) => m a -> m [a] -> m[a]
--	      func m acc
--	       = do x <- m
--		    xs <- acc
--		    return (x : xs)

 --sq3 ms = foldr func (return []) ms
--	where
--	      func :: (Monad m) => m a -> m [a] -> m[a]
--	      func m acc = m : acc
 
 --sq4 [] = return []
 --sq4 (m:ms) = return (a:as)
 --   where 
  --      a <- m
  --      as <- sq4 ms

 sq5 ms = foldr func (return []) ms
     where
	   func :: (Monad m) => m a -> m [a] -> m[a]
	   func m acc
	       = do x <- m
		    xs <- acc
		    return (x : xs)

 sq6 [] = return []
 sq6 (m:ms)
   = m >>=
       \a ->
	do as <- sq6 ms
           return (a : as)

 --sq7 [] = return []
 --sq7 (m:ms) = m >>= \a ->
  --  as <- sq7 ms
  --  return (a:as)

 sq8 [] = return []
 sq8 (m:ms)
  = do a <- m
       as <- sq8 ms
       return (a:as)

 fM a = l a
   where l = \ x -> putStrLn x >> return (x)

 mapM1 f as = sq1 (map f as)

 mapM2 f [] = return []
 mapM2 f (a : as)
  = f a >>= \ b -> mapM2 f as >>= \ bs -> return (b: bs)

 mapM3 f as = sequence2 (map f as)             --

 --mapM4 f [] = return []
 --mapM4 f (a:as)
 -- = f a >> \b -> mapM4 f as >> \ bs -> return (b:bs)

 --mapM5 f [] = return []
 --mapM5 f (a:as) =
  --   do
--	f a -> b
 --       mapM5 f as -> bs
  --      return (b:bs)

 mapM6 f [] = return []
 mapM6 f (a : as) 
  = do b <- f a
       bs <- mapM6 f as
       return (b : bs)

 mapM7 f [] = return []
 mapM7 f (a : as)
  = f a >>=
      \ b ->
	do bs <- mapM7 f as
	   return (b : bs)

 mapM8 f [] = return []           --
 mapM8 f (a : as)
  = f a >>=
      \ b ->
	do bs <- mapM8 f as
	   return (bs ++ [b])


 fP a = return (a > 0)


 filter1 _ [] = return []
 filter1 p (x : xs)
  = do flag <- p x
       ys <- filter1 p xs
       return (x : xs) 

 filter2 _ [] = return []
 filter2 p (x : xs)
  = do flag <- p x
       ys <- filter2 p xs
       if flag then return (x : ys) else return ys

 filter3 _ [] = return []
 filter3 p (x : xs)
  = do ys <- filter3 p xs
       if p x then return (x : ys) else return ys

 filter4 _ [] = return []
 filter4 p (x : xs)
  = do flag <- p x
       ys <- filter4 p xs
       if flag then return ys else return (x : ys)

 foldLeftM f a [] = return (a)
 foldLeftM f a (x : xs) 
   = do result <- f a x
        foldLeftM f ( result ) xs

 -- foldLeftM (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell" >>= \r -> putStrLn r

 foldRightM f z [] = return (z)
 foldRightM f z (x : xs)
   = do result <- foldRightM f z xs
	f x result

 -- foldRightM (\a b -> putChar a >> return (a : b)) [] (show [1, 3..10]) >>= \r -> putStrLn r

 m3 :: a -> IO a
 m3 a = return (a)
 m4 a = return (a + 1) 

 ff a = a * a


 liftM1 f m
   = do x <- m
 	return (f x)

 liftM2 f m = m >>= \a -> f a		 		--

 liftM3 f m = m >>= \a -> return (f a)

 liftM4 f m = return (f m)                              --

 liftM5 f m = m >>= \a -> m >>= \b -> return (f a)

 liftM6 f m = m >>= \a -> m >>= \b -> return (f b)
 
 liftM7 f m = mapM f [m]		                --

 liftM8 f m = m >> \ a -> return (f a)                  -- 

 
