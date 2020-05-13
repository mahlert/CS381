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


bbbox :: Shape -> BBox
bbox X = (1, 1)
bbox (TD s1 s2) | s1i >= s2i = (s1i, s1j + s2j)
                 | s1i < s2i  = (s2i, s1j + s2j)
                 where (s1i, s1j) = bbox s1
                       (s2i, s2j) = bbox s2

bbox (LR s1 s2) | s1j >= s2i = (s1i + s2i, s1j)
                 | s1j < s2i  = (s1i + s2i, s2j)
                 where (s1i, s1j) = bbox s1
                       (s2i, s2j) = bbox s2


--test case: bbbox(LR(TD X (LR X X)) X) should return (3,2)
--test case: bbbox (LR (TD (TD X X) X) X) should return (2,3)

