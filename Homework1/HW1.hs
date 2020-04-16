
type Contents a = (a, Int)
type Bag a = [Contents a]
b1 :: Bag Int
b2 :: Bag Int
b3 :: Bag Int
-- { (1, 1, 2, 2, 2, 3, 3, 3, 3}
b1 = [(1, 2), (2, 3), (3, 4)]
b2 = [(1,2), (2,3)]
b3 = [(2,3)]

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

