
 --import Prelude hiding ((||))

 --False || False = False        
 --_ || _ = True				--X

 --False || b = b
 --True || _ = True				--X

 --b || c
  -- | b == c = True
  -- | otherwise = False			

 --b || c
  -- | b == c = b
  -- | otherwise = True				--X

 --b || False = b
 --_ || True = True				--X

 --b || c
  -- | b == c = c
  -- | otherwise = True				--X

 --b || True = b
 --_ || True = True

 --False || False = False
-- False || True = True
-- True || False = True
 --True || True = True				--X
