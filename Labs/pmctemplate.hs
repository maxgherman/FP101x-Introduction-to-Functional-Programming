module Lab5 where

import Control.Monad

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action 
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================

action :: Concurrent a -> Action
action (Concurrent f) = f (\_ -> Stop)



-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
stop = Concurrent (\_ -> Stop)


-- ===================================
-- Ex. 2
-- ===================================

atom :: IO a -> Concurrent a
atom m = Concurrent(\a -> Atom (m >>= \c -> return (a c)))

-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork a = Concurrent (\c -> Fork (action a) (c ()))

par :: Concurrent a -> Concurrent a -> Concurrent a
par (Concurrent c1) (Concurrent c2) = Concurrent(\c -> Fork(c1 c ) (c2 c))


-- ===================================
-- Ex. 4
-- ===================================

instance Monad Concurrent where
    return x = Concurrent (\c -> c x)
-- (>>=) :: Concurrent a -> (a -> Concurrent b) -> Concurrent b
    (Concurrent f) >>= g = Concurrent(\x -> f (\a -> inner (g a) x ))
				where inner (Concurrent t) = t


-- ===================================
-- Ex. 5
-- ===================================

roundRobin [] = return ()
roundRobin (x:xs) = case x of
	Atom ia	    -> ia >>= \a -> roundRobin (xs ++ [a])
	Fork a1 a2  -> roundRobin (xs ++ [a1,a2])
	Stop	    -> roundRobin xs


-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

