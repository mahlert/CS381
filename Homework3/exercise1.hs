type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP


type Stack = [Int]

type D = Stack -> Stack

sem :: Prog -> D
sem [] y    = y
sem(x:xs) y = sem xs (semCmd x y)

