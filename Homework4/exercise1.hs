type Prog = [Cmd]

data Cmd = LD Int | ADD | MULT | DUP | INC | SWAP | POP Int
--deriving Show

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





