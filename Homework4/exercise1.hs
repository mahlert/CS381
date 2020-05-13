-- Exercise 1 part a --

type Prog = [Cmd]

data Cmd = LD Int | ADD | MULT | DUP | INC | SWAP | POP Int deriving Show

type Rank    = Int
type CmdRank = (Int, Int)

rankC :: Cmd -> CmdRank
rankC (LD i)  = (0,1)
rankC ADD     = (2,1)
rankC MULT    = (2,1)
rankC DUP     = (1,2)
rankC INC     = (1,1)
rankC SWAP    = (2,2)
rankC (POP i) = (i,0)

rankP :: Prog -> Maybe Rank
rankP [] = Just 0
rankP p = rank p 0

rank :: Prog -> Rank -> Maybe Rank
rank [] r     |     r >= 0       = Just r
rank (p:ps) r | under >= 0       = rank ps (under + adds)
              where (subs, adds) = rankC p
                    under        = r - subs
rank _ _      = Nothing

-- Exercise 1 Part b --

type Stack = [Int]
type D = Stack -> Stack
data St = A Stack deriving Show

sem :: Prog -> D
sem [] y    = y
sem(x:xs) y = sem xs (semCmd x y)

semCmd :: Cmd -> D
semCmd (LD i)  stack = (i:stack)
semCmd ADD     stack = ((last stack) + ((last . init) stack)):(drop 2 stack)
semCmd MULT    stack = ((last stack) * ((last . init) stack)):(drop 2 stack)
semCmd DUP     stack = (last stack):stack
semCmd INC     stack = (last stack)+1:(drop 1 stack)
semCmd SWAP    stack = ((last . init) stack):(last stack):(drop 2 stack)
semCmd (POP i) stack = (drop i stack)

typeCorrect :: Prog -> Bool
typeCorrect e = rankP e /= Nothing

semStatTC :: Prog -> Maybe Stack
semStatTC p | typeCorrect p = Just(sem p [])
            | otherwise     = Nothing



