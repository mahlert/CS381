main = print (map width [Pt (4,4), Circle(5,5) 3, Rect (3,3) 7 2])

type Number = Int

type Point = (Number,Number)
type Length = Number

data Shape = Pt Point
            | Circle Point Length
            | Rect Point Length Length
            deriving Show

type Figure = [Shape]
type BBox = (Point,Point)

width :: Shape -> Length
width (Pt _) = 0
width (Circle _ r) = 2 * r
width (Rect _ l _) = l


bbox :: Shape -> BBox
bbox (Pt (x,y)) = ((x,y), (x,y))
bbox (Circle (x,y) r) = ((x-r,y-r), (x+r,y+r))
bbox (Rect (x,y) l w) = ((x,y), (x+l,y+w))

minX :: Shape -> Number
minX (Pt (x,y)) = x
minX (Circle (x,y) r) = (x-r)
minX (Rect (x,y) l w) = x

minY :: Shape -> Number
minY (Pt (x,y)) = y
minY (Circle (x,y) r) = (y-r)
minY (Rect (x,y) l w) = y

maxX :: Shape -> Number
maxX (Pt (x,y)) = x
maxX (Circle (x,y) r) = (x+r)
maxX (Rect (x,y) l w) = x+l

maxY :: Shape -> Number
maxY (Pt (x,y)) = y
maxY (Circle (x,y) r) = (y+r)
maxY (Rect (x,y) l w) = y+w

addPt :: Point -> Point -> Point
addPt (x,y) (dx, dy) = (x+dx,y+dy)

move :: Shape -> Point -> Shape
move (Pt (x,y)) (dx,dy) = (Pt (addPt (x,y) (dx,dy)))
move (Circle (x,y) r) (dx,dy) = (Circle (addPt (x,y) (dx,dy)) r)
move (Rect (x,y) l w) (dx,dy) = (Rect (addPt (x,y) (dx,dy)) l w)

moveToX :: Number -> Shape -> Shape
moveToX n (Pt (x,y))        = (Pt (n,y))
moveToX n (Circle (x,y) r)  = (Circle (n,y) r)
moveToX n (Rect (x,y) l w)  = (Rect (n,y) l w)

alignLeft :: Figure -> Figure
alignLeft [] = []
alignLeft (s:ss) = [(moveToX (minX s) s)] ++ alignLeft ss

inside :: Shape -> Shape -> Bool
inside (Pt (x,y)) (Pt (a,b)) = x==a  && y==b
--side (Shape s) (Pt (a,b)) = a>=(minX (Shape s))
inside (Pt (x,y)) (Circle (a,b) r) = x>=(minX (Circle (a,b) r)) &&
                                     x<=(maxX (Circle (a,b) r)) &&
                                     y>=(minY (Circle (a,b) r)) &&
                                     y<=(maxY (Circle (a,b) r))
inside (Circle (a,b) r) (Pt (x,y)) = x>=(minX (Circle (a,b) r)) &&
                                     x<=(maxX (Circle (a,b) r)) &&
                                     y>=(minY (Circle (a,b) r)) &&
                                     y<=(maxY (Circle (a,b) r))
inside (Circle (x,y) r) (Circle (a,b) s) =  x>=(minX (Circle (a,b) r)) &&
                                            x<=(maxX (Circle (a,b) r)) &&
                                            y>=(minY (Circle (a,b) r)) &&
                                            y<=(maxY (Circle (a,b) r))
inside (Rect (x,y) l1 w1) (Rect (a,b) l2 w2) = x>=(minX (Rect (a,b) l2 w2)) &&
                                               x<=(maxX (Rect (a,b) l2 w2)) &&
                                               y>=(minY (Rect (a,b) l2 w2)) &&
                                               y<=(maxY (Rect (a,b) l2 w2))



f = [Pt (4,4), Circle(5,5) 3, Rect (3,3) 7 2]
