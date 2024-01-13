newtype Box a = MkBox a deriving Show

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w 

newtype MyTriple a = MyTriple (a,a,a) deriving Show 

instance Functor MyTriple where 
    fmap f (MyTriple (x,y,z)) = MyTriple(f x, f y, f z)

instance Applicative MyTriple where 
    pure x = MyTriple (x,x,x)
    (MyTriple (f,g,h)) <*> (MyTriple (x,y,z)) = MyTriple (f x, g y, h z)

data Tree2 a = EmptyT2 | Leaf a | Node (Tree2 a) a (Tree2 a) deriving Show

instance Functor Tree2 where 
    fmap _ EmptyT2 = EmptyT2
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Applicative Tree2 where
    pure = Leaf 
    EmptyT2 <*> _ = EmptyT2
    _ <*> EmptyT2 = EmptyT2
    (Leaf f) <*> (Leaf x) = Leaf (f x)
    (Leaf f) <*> (Node l x r) = Node (fmap f l) (f x) (fmap f r)
    (Node fl f fr) <*> (Leaf x) = Node (fl <*> (Leaf x)) (f x) (fr <*> (Leaf x))
    (Node fl f fr) <*> (Node l x r) = Node (fl <*> l) (f x) (fr <*> r)
