
 import Data.List
 import Data.Char
 import Unsafe.Coerce

 data Nat = Zero
	  | Succ Nat
	  deriving Show

 nat2int1 Zero = 0
 nat2int1 (Succ n) = nat2int1 n + 1

 nat2int2 (Succ n) = nat2int2 n + 1
 nat2int2 Zero = 0

 nat2int3 n = nat2int3 n		--

 nat2int4 (Succ n) = 1 + nat2int4 n
 nat2int4 Zero = 0

 nat2int5 Zero = 1
 nat2int5 (Succ n) = (1 + nat2int5 n) - 1  --

 nat2int6 = head . m
   where m Zero = [0]
 	 m (Succ n) = [sum [x | x <- (1 : m n)]]

 nat2int7 :: Nat -> Integer
 nat2int7 = \ n -> genericLength [c | c <- show n, c == 'S']

 --nat2int8 :: Nat -> Integer
 --nat2int8 = \ n -> length [c | c <- show n, c == 'S']


 int2nat1 0 = Zero
 int2nat1 (n + 1) = Succ (int2nat1 n)

 int2nat2 0 = Succ Zero
 int2nat2 n = (Succ (int2nat2 n))     --

 int2nat3 n 
  = product [(unsafeCoerce c) :: Integer | c <- show n]     --

 int2nat4 n = int2nat4 n               --

 int2nat5 (n + 1) = Succ (int2nat5 n)
 int2nat5 0 = Zero

 int2nat6 (n+1) = let m = int2nat6 n in Succ m
 int2nat6 0 = Zero

 int2nat7 = head . m
   where {
	 ; m 0 = [0]
	 ; m (n + 1) = [sum [x | x <- (1 : m n)]]     --
	 }

 --int2nat8 :: Integer -> Nat
 --int2nat8 = \ n -> genericLength [c | c <- show n, isDigit c]


 nat n = int2nat1 n
 nat1 = nat 2
 nat2 = nat 2
 nat3 = nat 3
 nat4 = nat 4
 nat5 = nat 5
 nat0 = nat 0


 fop :: Num a => (a -> a -> a) -> a -> a -> a
 fop op a b = op a b 
 
 natPair f op n m = nat2int1 (f n m) == (fop op (nat2int1 n) (nat2int1 m))
  
 equalsAdd f n m = natPair f (+) n m

 equalsMul f n m = natPair f (*) n m

 add1 Zero n  = n
 add1 (Succ m) n = Succ (add1 n m)

 add2 (Succ m) n = Succ (add2 n m)
 add2 Zero n = n

 add3 Zero n = Zero
 add3 (Succ m) n = Succ (add3 m n)		--

 add4 (Succ m) n = Succ (add4 m n)		--
 add4 Zero n = Zero

 add5 n Zero = Zero
 add5 n (Succ m) = Succ (add5 n m)		--

 add6 n (Succ m) = Succ (add6 n m)		--
 add6 n Zero = Zero

 add7 n Zero = n
 add7 n (Succ m) = Succ (add7 m n)

 add8 n (Succ m) = Succ (add8 m n)
 add8 n Zero = n

 mult1 Zero Zero = Zero
 mult1 m (Succ n) = add1 m (mult1 m n)
 
 mult2 m Zero = Zero
 mult2 m (Succ n) = add1 m (mult2 m n)

 mult3 m Zero = Zero
 mult3 m (Succ n) = add1 n (mult3 m n)

 mult4 m Zero = Zero
 mult4 m n = add1 m (mult4 m (Succ n))




