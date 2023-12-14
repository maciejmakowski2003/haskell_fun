data MyInt = MkMyInt Int--new type

instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2 

instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2 

instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromIntegral int)

instance Show MyInt where
  show (MkMyInt i) = "MkMyInt " ++ show i

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) 

instance Eq a => Eq (BinTree a) where 
    (==) EmptyBT EmptyBT = True 
    (==) (NodeBT x lt1 rt1) (NodeBT y lt2 rt2) = x==y && lt1 == lt2 && rt1 == rt2 
    (==) _ _ = False

data Cart3DVec a = Cart3DVec a a a 

instance Eq a => Eq (Cart3DVec a) where 
    (==) (Cart3DVec x1 y1 z1) (Cart3DVec x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2 

