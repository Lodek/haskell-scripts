data Color = Red | Yellow | Green | Blue | Orange | White deriving (Show, Eq)
data Orientation = Left | Right | Up | Down | Back | Front
data Face = Face Color Color Color Color deriving (Show, Eq)
data Cube = Cube {front :: Face, right :: Face, up :: Face, back :: Face, left :: Face, down :: Face} deriving (Show, Eq)

--Face indexing function
--Probably unncessary
sqr :: Int -> Face -> Color
sqr n (Face a b c d) 
  | n == 0 = a
  | n == 1 = b
  | n == 2 = c
  | n == 3 = d


reorientCube :: Orientaton -> Cube -> Cube
reorientCube o cube@(Cube {front=f, back=b, left=l, right=r, up=u, down=d})
  | o == Left = Cube {front=l, back=r, up=u, down=d, left=b, right=f}
  | o == Right = Cube {front=r, back=l, up=u, down=d, left=f, right=b}
  | o == Up = Cube {front=u, back=d, up=b, down=f, left=l, right=r}
  | o == Down = Cube {front=d, back=u, up=f, down=b, left=l, right=r}
  | o == Back = Cube {front=b, back=f, up=u, down=d, left=r, right=l}
  | o == Front = cube
  
cubePOVLeft = reorientCube Left

cubePOVRight = reorientCube Right

cubePOVBack = reorientCube Back

cubePOVUp = reorientCube Up

cubePOVDown = reorientCube Down


frontRotation :: Cube -> Cube --clockwise 90 deg front rotation
frontRotation (Cube {front=f, back=b, left=l, right=r, up=u, down=d}) = Cube {front=f', back=b, left=l', right=r', up=u', down=d'}
  where u' = Face (sqr 0 u) (sqr 1 u) (sqr 3 l) (sqr 1 l)
        d' = Face (sqr 2 r) (sqr 0 r) (sqr 2 d) (sqr 3 d)
        l' = Face (sqr 0 l) (sqr 0 d) (sqr 2 l) (sqr 1 d)
        r' = Face (sqr 2 u) (sqr 1 r) (sqr 3 u) (sqr 3 r)
        f' = Face (sqr 2 f) (sqr 0 f) (sqr 3 f) (sqr 1 f)

leftRotation = cubePOVRight. frontRotation . cubePOVLeft
rightRotation = cubePOVLeft . frontRotation . cubePOVRight
downRotation = cubePOVUp . frontRotation . cubePOVDown
upRotation = cubePOVDown . frontRotation . cubePOVUp
backRotation = cubePOVBack . frontRotation . cubePOVBack



solvedFace :: Color -> Face
solvedFace c = Face c c c c

solvedCube = Cube {front=solvedFace Green, right=solvedFace Red,
                   left=solvedFace Orange, up=solvedFace White,
                   down=solvedFace Yellow, back=solvedFace Blue}
