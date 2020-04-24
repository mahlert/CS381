--2a: define the abstract synaxt for the above languge as a Haskell data type
data Pair = Pair Int Int
data Circuit = Circuit Gates Links
data Gates = Gate Int GateFn Gates | GNone
data GateFn = And | Or | Xor | Not
data Links = Link Pair Pair Links | LNone

--2b: represent the half adder circuit in abstract syntax, that is,
--as a Haskell data type
halfaddr = Circuit (Gate 1 Xor (Gate 2 And GNone))
                   (Link (Pair 1 1) (Pair 2 1) (Link (Pair 1 2) (Pair 2 2) LNone))
