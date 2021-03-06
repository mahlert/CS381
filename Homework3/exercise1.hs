type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
           deriving Show

type Stack = [Int]

type D = Stack -> Stack

sem :: Prog -> D
sem [] y    = y
sem(x:xs) y = sem xs (semCmd x y)

semCmd :: Cmd -> D
--semCmd (LD x) stack = x:stack
--semCmd (ADD) stack  = (head stack + head (tail stack)):(tail (tail stack))
--semCmd (MULT) stack = (head stack * head (tail stack)):(tail (tail stack))
--semCmd (DUP) stack  = (head stack):stack

semCmd (LD i) stack = (i:stack)
semCmd ADD    stack = ((last stack) + ((last . init) stack)):(drop 2 stack)
semCmd MULT   stack = ((last stack) * ((last . init) stack)):(drop 2 stack)
semCmd DUP    stack = (last stack):stack

