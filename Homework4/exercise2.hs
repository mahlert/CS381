data Shape = X
	   | TD Shape Shape
	   | LR Shape Shape
	   deriving Show

type BBox = (Int,Int)

--data Type = Int | BBox | TypeError
--	  deriving (Eq,Show)

--bbox :: Shape -> Type
--bbox X 				   		= Int
--bbox (TD s1 s2) | bbox s1==Int && bbox s2==Int 	= BBox
--bbox (LR s1 s2) | bbox s1==Int && bbox s2==Int	= BBox
--bbox _			= TypeError


bbox :: Shape -> BBox
bbox X = (1, 1)
bbox (TD s1 s2)  | s1i >= s2i = (s1i, s1j + s2j)
                 | s1i < s2i  = (s2i, s1j + s2j)
                 where (s1i, s1j) = bbox s1
                       (s2i, s2j) = bbox s2

bbox (LR s1 s2)  | s1j >= s2i = (s1i + s2i, s1j)
                 | s1j < s2i  = (s1i + s2i, s2j)
                 where (s1i, s1j) = bbox s1
                       (s2i, s2j) = bbox s2


rect :: Shape -> Maybe BBox
rect X = Just (1, 1)
rect (TD s1 s2) | Just s1i == Just s2i = Just (s1i, s1j + s2j)
                | otherwise = Nothing
                where Just (s1i, s1j) = rect s1
                      Just (s2i, s2j) = rect s2

rect (LR s1 s2) | Just s1j == Just s2j = Just (s1i + s2i, s1j)
                | otherwise = Nothing
                where Just (s1i, s1j) = rect s1
                      Just (s2i, s2j) = rect s2



--test case bbox: bbbox(LR(TD X (LR X X)) X) should return (3,2)
--test case bbox: bbbox (LR (TD (TD X X) X) X) should return (2,3)

--test case rect: rect(TD X X) should return Just (1,2)
--test case rect: rect(TD(LR X X)(LR X X)) should return Just 2,2)

