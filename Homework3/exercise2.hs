data Cmd = Pen Mode
	 | MoveTo Int Int
	 | Seq Cmd Cmd
	 deriving Show

data Mode = Up | Down deriving Show
type State = (Mode, Int, Int)
type Line = (Int, Int, Int, Int)
type Lines = [Line]

semS :: Cmd -> State -> (State, Lines)
semS (Pen p) (x,y,z) = ((p,y,z),[(y,z,y,z)])
semS (MoveTo a b) (x,y,z) = ((x,a,b), l1)
     where l1 = [(y,z,a,b)]
semS (Seq c c') (x,y,z) = (s2,l3)
     where (s1,l1) = semS c (x,y,z)
     	   (s2,l2) = semS c' s1
	   l3 = l1 ++ l2

sem' :: Cmd -> Lines
sem' (x) = l1
     where (s1,l1) = semS x (Up,0,0)
