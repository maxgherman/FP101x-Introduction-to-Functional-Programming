
 --import Prelude hiding ((&&))
 
 --True && True = True
 --_ && _ = False						--X

 --a && b = if a then if b then True else False else False	--X

 --a && b = if not (a) then not (b) else True

 --a && b = if a then b

 -- a && b = if a then if b then False else True else False

-- a && b = if a then b else False				--X

-- a && b = if b then a else False				--X
