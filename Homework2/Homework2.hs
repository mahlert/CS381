--2a: define the abstract synaxt for the above languge as a Haskell data type
type circuit = (gates, links)
type gates = [(Int, gateFn)]
data gateFn = and | or | xor | not
type links = [link]
data link = L (Int, Int) (Int, Int)

--2b: represent the half adder circuit in abstract syntax,
--that is,
--as a Haskell data type
([[1, xor], [2, and]], [L, (1,1) (2,1), L (1,2) (2,2)])
