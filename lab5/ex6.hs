{-# LANGUAGE DeriveFunctor #-}
newtype Box a = MkBox a deriving Show

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)

newtype Box2 a = MkBox2 a deriving (Show, Functor)

data MyList a = EmptyList
              | Cons a (MyList a) deriving Show

instance Functor MyList where
  fmap _ EmptyList    = EmptyList
  fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)

newtype Pair b a = Pair { getPair :: (a,b) } -- fmap should change the first element
data Tree2 a = EmptyT2 | Leaf a | Node (Tree2 a) a (Tree2 a) deriving Show
data GTree a = GLeaf a | GNode [GTree a] deriving Show

instance Functor (Pair b) where
  fmap f (Pair (x,y)) = Pair (f x, y)

instance Functor Tree2 where 
  fmap _ EmptyT2 = EmptyT2
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Functor GTree where
  fmap f (GLeaf x) = GLeaf (f x)
  
prod2 x y = x*y 
prod3 x y z = x*y*z 

prodOfElems :: Num b => BinTree(Either a b) -> Either a b
prodOfElems EmptyBT = Right 1 
prodOfElems (NodeBT (Left _) l r) = prod2 <$> prodOfElems l <*> prodOfElems r
prodOfElems (NodeBT rel l r) = prod3 <$> prodOfElems l <*> rel <*> prodOfElems r
