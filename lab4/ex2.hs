-- product type example (one constructor)
data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

--xCoord'' :: Cart2DVec'' a -> a
--xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

--yCoord'' :: Cart2DVec'' a -> a
--yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal
--xCoord'' $ MkCart2DVec'' {x=1, y=2}
--xCoord'' $ MkCart2DVec'' 1 2

-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

data Cart3DVec a = Cart3DVec a a a 

xCoord3D :: Cart3DVec a -> a
xCoord3D (Cart3DVec x _ _) = x 

data Cart3DVec' a = Cart3DVec' {x'::a, y'::a, z'::a} 
polarToCartesian :: Floating a => a -> a -> a -> Cart3DVec' a
polarToCartesian r theta phi =
  Cart3DVec' (r * sin phi * cos theta) (r * sin phi * sin theta) (r * cos phi)

data Shape = Circle Float | Rectangle Float Float 
area :: Shape -> Float  
area (Circle r) = r^2 * pi 
area (Rectangle a b) = a*b 

data Tree a = EmptyT | Node a (Tree a) (Tree a)
              deriving Show 

rootValue :: Tree a -> a 
rootValue EmptyT = error "no root" 
rootValue (Node e lt rt) = e

