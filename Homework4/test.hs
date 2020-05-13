data Shape = X
	   | TD Shape Shape
	   | LR Shape Shape
	   deriving Show

type BBox = (Int,Int)

data Type = Int | BBox | TypeError
	  deriving (Eq,Show)

bbox :: Shape -> Type
bbox X 				   		= Int
bbox (TD s1 s2) | bbox s1==Int && bbox s2==Int 	= BBox
bbox (LR s1 s2) | bbox s1==Int && bbox s2==Int	= BBox
bbox _						= TypeError
