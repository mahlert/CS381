--import Expr
--import TypeCheck
--import Data.Map (fromListWith, toList)

type Bag a = [(a, Int)]
b1 :: Bag Int
b2 :: Bag Int
b3 :: Bag Int
-- { (1, 1, 2, 2, 2, 3, 3, 3, 3}
b1 = [(1, 2), (2, 3), (3, 4)]
b2 = [(1,2), (2,3)]
b3 = [(2,3)]
--b2 = [("abc", 2), ("def", 1)] Im a clown and this didnt work

-- a. inserts an element into a multiset
ins :: Eq a => a -> Bag a -> Bag a
ins x [] = [(x, 1)]
-- if x equals the topElement return the bag with x's counter ++
ins x ((topElement, counter):xs) | x == topElement = ((topElement, counter + 1):xs)
                                 -- | otherwise = xs ++ (ins x xs)
                                 | otherwise = ((topElement, counter) : ins x xs)



-- b. Removes an element from a multset
del :: Eq a => a -> Bag a -> Bag a
del x [] = []
del x ((topElement, counter): xs) | x == topElement = ((topElement, counter - 1):xs)
                                  | otherwise = []

-- c. Produces a multiset represenation based off of given values
bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = ins x (bag xs)

-- d. determines whether or not the 1st argument is containted in the 2nd
subbag :: Eq a => Bag a -> Bag a -> Bool
--subbag [] [] = True
subbag [] _ = True
--subbag _  [] = False
subbag (bagItem1:xs) bag2 = case checkbag bag2 bagItem1 of
                            True -> subbag xs bag2
                            False -> False

-- checks all elements in givin in the bag and returns true if stuff in bag 1
-- is in bag 2
checkbag :: Eq a => Bag a -> (a, Int) -> Bool
checkbag [] (_,_)  = False
checkbag ((bagItem, bagItemCount):xs) (item, count) | bagItem == item && count == bagItemCount = True
                                                    | otherwise = checkbag xs (item, count)

-- ELP
-- e. computes the interesection of two multisets
isbag :: Eq a => Bag a -> Bag a -> Bag a
isbag bag1 [] = []
isbag [] bag2 = []

--HELP (for part e)
insecBag :: Eq a => Bag a -> Bag a -> [a] -> Bag a
insecBag [] _ newBag = bag newBag
insecBag ((bagItem1, x):xs) bag2 newBag = case checkbag bag2 (bagItem1, x) of
                                        True -> insecBag xs bag2 (bagItem1:newBag)
                                        False -> insecBag xs bag2 newBag

-- f. computes the number of elements contained in a bag
size :: Bag a -> Int
size [] = 0
size ((element, counter):xs) = counter + size xs

