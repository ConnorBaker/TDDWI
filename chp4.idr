import Data.Vect

-- 4.1
%default total

data Direction = North | East | South | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East  = South
turnClockwise South = West
turnClockwise West  = North

||| Represents shapes
data Shape : Type where 
  Circle    : (radius : Double)                      -> Shape
  Triangle  : (base   : Double) -> (height : Double) -> Shape
  Rectangle : (length : Double) -> (height : Double) -> Shape

%name Shape shape, shape1, shape2

area : Shape -> Double
area (Circle    radius       ) = radius * radius * pi
area (Triangle  base   height) = base   * height * 0.5
area (Rectangle length height) = length * height

data Picture : Type where
  Primitive : Shape                                     -> Picture
  Combine   : Picture                        -> Picture -> Picture
  Rotate    : (deg : Double)                 -> Picture -> Picture
  Translate : (x   : Double) -> (y : Double) -> Picture -> Picture

%name Picture pic, pic1, pic2

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture =
  Combine
    (Translate 5 5 rectangle)
    (Combine (Translate 35 5 circle)
      (Translate 15 25 triangle))

pictureArea : Picture -> Double
pictureArea (Primitive s      ) = area s
pictureArea (Combine   p1 p2  ) = pictureArea p1 + pictureArea p2
pictureArea (Rotate    _  p   ) = pictureArea p
pictureArea (Translate _  _  p) = pictureArea p

data Tree e = Empty | Node (Tree e) e (Tree e)

%name Tree tree, tree1

insert : Ord e => e -> Tree e -> Tree e
insert x (Empty     ) = Node Empty x Empty
insert x (Node l y r) =
  case compare x y of
    LT => Node (insert x l) y (r         )
    EQ => Node (l         ) y (r         )
    GT => Node (l         ) y (insert x r)

-- Exercises listed in reverse order to keep the buffer mid-screen
data Expr : Type where
  EInt : Int          -> Expr
  EAdd : Expr -> Expr -> Expr
  ESub : Expr -> Expr -> Expr
  EMul : Expr -> Expr -> Expr

eval : Expr -> Int
eval (EInt x  ) = x
eval (EAdd x y) = eval x + eval y
eval (ESub x y) = eval x - eval y
eval (EMul x y) = eval x * eval y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe (Just  x) (Just  y) = Just (max x y)
maxMaybe _         _         = Nothing

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive t@(Triangle _ _)     ) = Just (area t)
biggestTriangle (Primitive _                    ) = Nothing
biggestTriangle (Combine   p1               p2  ) = max (biggestTriangle p1) (biggestTriangle p2)
biggestTriangle (Rotate    _                p   ) = biggestTriangle p
biggestTriangle (Translate _                _  p) = biggestTriangle p

testPic1 : Picture
testPic1 =
  Combine
    (Primitive (Triangle 2 3))
    (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 =
  Combine
    (Primitive (Rectangle 1 3))
    (Primitive (Circle 4))

listToTree : Ord a => List a -> Tree a
listToTree = foldr insert Empty

treeToList : Ord a => Tree a -> List a
treeToList (Empty     ) = []
treeToList (Node l x r) = treeToList l ++ [x] ++ treeToList r

-- 4.2
data PowerSource = Petrol | Pedal

data Vehicle : PowerSource -> Type where
  Unicycle   :                 Vehicle Pedal
  Bicycle    :                 Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Car        : (fuel : Nat) -> Vehicle Petrol
  Bus        : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels (Unicycle       ) = 1
wheels (Bicycle        ) = 2
wheels (Motorcycle     ) = 2
wheels (Car        fuel) = 4
wheels (Bus        fuel) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle _) = Motorcycle 50
refuel (Car        _) = Car        100
refuel (Bus        _) = Bus        200

vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
vectTake (Z  ) _         = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

sumEntries : Num a => (idx : Nat) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} idx xs ys = 
  case natToFin idx n of
    Nothing => Nothing
    Just  m => Just (index m xs + index m ys) 
