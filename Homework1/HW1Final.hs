module HW1types where

import Data.List (nub,sort)

norm :: Ord a => [a] -> [a]
norm = sort . nub

type Node = Int
type Edge = (Node,Node)
type Graph = [Edge]
type Path = [Node]

type Contents a = (a, Int)
type Bag a = [Contents a]

type Number = Int
type Point = (Number,Number)
type Length = Number

data Shape = Pt Point
            | Circle Point Length
            | Rect Point Length Length
            deriving Show

type Figure = [Shape]
type BBox = (Point,Point)

b1 :: Bag Int
b2 :: Bag Int
b3 :: Bag Int
-- { (1, 1, 2, 2, 2, 3, 3, 3, 3}
b1 = [(1, 2), (2, 3), (3, 4)]
b2 = [(1,2), (2,3)]
b3 = [(2,3)]

-- Exercise 1. Programming with Lists

-- a. inserts an element into a multiset
ins :: Eq a => a -> Bag a -> Bag a
ins x [] = [(x, 1)]
-- if x equals the topElement return the bag with x's counter ++
ins x ((topElement, counter):xs) | x == topElement = ((topElement, counter + 1):xs)
                                 | otherwise = ((topElement, counter) : ins x xs)



-- b. Removes an element from a multset
del :: Eq a => a -> Bag a -> Bag a
del x [] = []
del x ((topElement, counter): xs) | x == topElement = ((topElement, counter - 1):xs)
                                  | otherwise = ((topElement, counter) : del x xs)

-- c. Produces a multiset represenation based off of given values
bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = ins x (bag xs)

-- d. determines whether or not the 1st argument is containted in the 2nd
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True
subbag (bagItem1:xs) bag2 = case checkbag bag2 bagItem1 of
                            True -> subbag xs bag2
                            False -> False

-- checks all elements in givin in the bag and returns true if stuff in bag 1
-- is in bag 2
checkbag :: Eq a => Bag a -> (a, Int) -> Bool
checkbag [] (_,_)  = False
checkbag ((bagItem, bagItemCount):xs) (item, count) | bagItem == item && count == bagItemCount = True
                                                    | otherwise = checkbag xs (item, count)


isbag :: Eq a => Bag a -> Bag a -> Bag a
isbag bag1 [] = []
isbag [] bag2 = []
isbag (x:xs) bag2 = addBag(insecBag x bag2) (isbag xs bag2)

--returns a bag that contains an item if the item is in the bag
insecBag :: Eq a => Contents a -> Bag a -> Bag a
insecBag a [] = []
insecBag (item, count) ((bagItem, bagItemCount):xs) | item == bagItem = [(item, min count bagItemCount)]
                                                    | otherwise = insecBag (item, count) xs

--adds items from bag1 into bag2 then retuns the new bag
addBag :: Eq a => Bag a -> Bag a -> Bag a
addBag bag1 [] = bag1
addBag [] bag2 = bag2
addBag (x:xs) bag2 = addBag xs (addToBag x bag2)

--takes an item of the bag and adds it to bag2 then returns the new bag
--if that item is already in the bag then inc the count
addToBag :: Eq a => Contents a -> Bag a -> Bag a
addToBag a [] = [a]
addToBag (item1, count1) ((bagItem, bagItemCount):xs) | item1 == bagItem = ((item1, count1 + bagItemCount):xs)
                                                      | otherwise = ((bagItem, bagItemCount):addToBag (item1, count1) xs)


-- f. computes the number of elements contained in a bag
size :: Bag a -> Int
size [] = 0
size ((element, counter):xs) = counter + size xs

-- Exercise 2. Graphs

-- a. Function that computes the list of nodes contained in a given graph.
nodes :: Graph -> [Node]
nodes [] = []
nodes ((x,y):xs) = norm (x : y : nodes xs)

-- b. Function that computes the list of successors for a node in a given graph.
suc :: Node -> Graph -> [Node]
suc a [] = []
suc a ((x,y):xs)
   | a == x = (y : (suc a xs))
   | a /= x = suc a xs

-- c. Function that remove sa node together with all of its incident edges from a graph.
detach :: Node -> Graph -> Graph
detach a [] = []
detach a ((x,y):xs)
   | a == x = detach a xs
   | a == y = detach a xs
   | otherwise = (x,y) : detach a xs

-- d. Function that creates a cycle of any given number.
cyc :: Int -> Graph
cyc c = take c (zip [1..c+1] [2..c+1])

--Exercise 3. Programming with Data Types

main = print (map width [Pt (4,4), Circle(5,5) 3, Rect (3,3) 7 2])

--part a--
width :: Shape -> Length
width (Pt _) = 0
width (Circle _ r) = 2 * r
width (Rect _ l _) = l

--part b--
bbox :: Shape -> BBox
bbox (Pt (x,y)) = ((x,y), (x,y))
bbox (Circle (x,y) r) = ((x-r,y-r), (x+r,y+r))
bbox (Rect (x,y) l w) = ((x,y), (x+l,y+w))

--part c - returns smallest x coord of shape--
minX :: Shape -> Number
minX (Pt (x,y)) = x
minX (Circle (x,y) r) = (x-r)
minX (Rect (x,y) l w) = x

--helper func - returns smallest y coord of shape--
minY :: Shape -> Number
minY (Pt (x,y)) = y
minY (Circle (x,y) r) = (y-r)
minY (Rect (x,y) l w) = y

--helper func - returns largest x coord of shape--
maxX :: Shape -> Number
maxX (Pt (x,y)) = x
maxX (Circle (x,y) r) = (x+r)
maxX (Rect (x,y) l w) = x+l

--helper func - returns largest y coord of shape--
maxY :: Shape -> Number
maxY (Pt (x,y)) = y
maxY (Circle (x,y) r) = (y+r)
maxY (Rect (x,y) l w) = y+w

--helper function to part d--
addPt :: Point -> Point -> Point
addPt (x,y) (dx, dy) = (x+dx,y+dy)

-- part d--
move :: Shape -> Point -> Shape
move (Pt (x,y)) (dx,dy) = (Pt (addPt (x,y) (dx,dy)))
move (Circle (x,y) r) (dx,dy) = (Circle (addPt (x,y) (dx,dy)) r)
move (Rect (x,y) l w) (dx,dy) = (Rect (addPt (x,y) (dx,dy)) l w)

--helper function to part e--
moveToX :: Number -> Shape -> Shape
moveToX n (Pt (x,y))        = (Pt (n,y))
moveToX n (Circle (x,y) r)  = (Circle (n,y) r)
moveToX n (Rect (x,y) l w)  = (Rect (n,y) l w)

-- part e --
alignLeft :: Figure -> Figure
alignLeft [] = []
alignLeft (s:ss) = [(moveToX (minX s) s)] ++ alignLeft ss

-- part f --
inside :: Shape -> Shape -> Bool
inside (Pt (x,y)) (Pt (a,b)) = x==a  && y==b
inside s1 s2 =  ((((minX s1)>=(minX s2) && (minX s1)<=(maxX s2)) ||
                ((maxX s1)>=(minX s2) && (maxX s1)<=(maxX s2))) &&
                (((minY s1)>=(minY s2) && (minY s1)<=(maxY s2)) ||
                ((maxY s1)>=(minY s2) && (maxY s1)<=(maxY s2))))
                ||
                ((((minX s1)<=(minX s2) && (maxX s1)>=(maxX s2)) ||
                ((minX s1)>=(minX s2) && (maxX s1)<=(maxX s2))) &&
                (((minY s1)<=(minY s2) && (maxY s1)>=(maxY s2)) ||
                ((minY s1)>=(minY s2) && (maxY s1)<=(maxY s2))))


f = [Pt (4,4), Circle(5,5) 3, Rect (3,3) 7 2]
