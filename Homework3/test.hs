data Cmd = Pen Mode
	 | MoveTo Int Int
	 | Seq Cmd Cmd
	 deriving Show

data Mode = Up | Down deriving Show
type State = (Mode, Int, Int)
type Line = (Int, Int, Int, Int)
type Lines = [Line]

semS :: Cmd -> State -> (State, Lines)
--Base Case 
--Change Pen Mode
semS (Pen p) (x,y,z) = ((p,y,z),[(y,z,y,z)])
--MoveTo
semS (MoveTo a b) (x,y,z) = ((x,y,z), [(y,z,a,b)])
--Sequence Cmd
semS (Seq c c') (x,y,z) = (s2,l2)
     where (s1,l1) = semS c (x,y,z)
     	   (s2,l2) = semS c' s1
