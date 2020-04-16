import Data.List (nub,sort)

type Node = Int
type Edge = (Node,Node)
type Graph = [Edge]
type Path = [Node]

norm :: Ord a => [a] -> [a]
norm = sort . nub

nodes :: Graph -> [Node]
nodes ((x,y):xs) = norm (x : y : nodes xs)
nodes _ = []

suc :: Node -> Graph -> [Node]
suc a [] = []
suc a ((x,y):xs)
   | a == x = (y : (suc a xs))
   | a /= x = suc a xs

detach :: Node -> Graph -> Graph
detach a [] = []
detach a ((x,y):xs)
   | a == x = detach a xs
   | a == y = detach a xs
   | otherwise = (x,y) : detach a xs

cyc :: Int -> Graph
cyc c = take c (zip [1..c+1] [2..c+1])
