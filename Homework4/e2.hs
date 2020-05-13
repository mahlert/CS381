data Shape = X
	   | TD Shape Shape
	   | LR Shape Shape
	   deriving Show

type BBox = (Int,Int)

data Type = Int | BBox | TypeError
	  deriving (Eq,Show)

bbox :: Shape -> Type
bbox X 				   								= BBox
bbox (TD s1 s2) | bbox s1==BBox && bbox s2==BBox 	= BBox
bbox (LR s1 s2) | bbox s1==BBox && bbox s2==BBox	= BBox
bbox _												= TypeError

{-
recttc :: Shape -> Type
recttc X 				   								= BBox
recttc (TD s1 s2) | recttc s1==BBox && recttc s2==BBox 	= BBox
recttc (LR s1 s2) | recttc s1==BBox && recttc s2==BBox	= BBox
recttc _												= TypeError

-}




rectTC :: Shape -> Type
rectTC X			= BBox
rectTC (TD s1 s2) = if rectTC s1==BBox && rectTC s2==BBox 
						then	if sameW s1 s2							
									then BBox
									else TypeError
					else TypeError
rectTC (LR s1 s2) = if rectTC s1==BBox && rectTC s2==BBox 
						then	if sameH s1 s2							
									then BBox
									
									else TypeError
					else TypeError
rectTC _			= TypeError



sameW :: BBox -> BBox -> Bool
sameW s1 s2 = if fst s1 == fst s2 
						then True
						else False


sameH :: BBox -> BBox -> Bool
sameH s1 s2 = if snd s1 == snd s2 
						then True
						else False










