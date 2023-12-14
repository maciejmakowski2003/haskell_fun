import Data.Sequence (Seq(Empty))
data BinIntTree = EmptyIntBT | IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt 

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int 
depthOfBT EmptyBT = 0  
depthOfBT (NodeBT x lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt) 

flattenBTPreorder :: BinTree a -> [a]
flattenBTPreorder EmptyBT = []
flattenBTPreorder (NodeBT x lt rt) =
  [x] ++ flattenBTPreorder lt ++ flattenBTPreorder rt

flattenBTInorder :: BinTree a -> [a]
flattenBTInorder EmptyBT = []
flattenBTInorder (NodeBT x lt rt) =
  flattenBTInorder lt ++ [x] ++ flattenBTInorder rt

flattenBTPostorder :: BinTree a -> [a]
flattenBTPostorder EmptyBT = []
flattenBTPostorder (NodeBT x lt rt) =
  flattenBTPostorder lt ++ flattenBTPostorder rt ++ [x]

mapBT :: (a -> b) -> BinTree a -> BinTree b 
mapBT f EmptyBT =  EmptyBT
mapBT f (NodeBT x lt rt) = NodeBT (f x) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a
insert val EmptyBT = NodeBT val EmptyBT EmptyBT 
insert val (NodeBT x lt rt) 
  | val<x = NodeBT x (insert val lt) rt 
  | val>x = NodeBT x lt (insert val rt) 
  | otherwise = NodeBT x lt rt

list2BST :: Ord a => [a] -> BinTree a 
list2BST [] = EmptyBT
list2BST (x:xs) = insert x (list2BST xs)

occurs :: Eq a => a -> BinTree a -> Int 
occurs _ EmptyBT = 0 
occurs val (NodeBT x lt rt)
  | x == val = 1 + occurs val lt + occurs val rt
  | otherwise = occurs val lt + occurs val rt 

elemOf :: Eq a => a -> BinTree a -> Bool 
elemOf val EmptyBT = False
elemOf val (NodeBT x lt rt) 
  | x == val = True
  | otherwise = elemOf val lt || elemOf val rt

reflect :: BinTree a -> BinTree a 
reflect EmptyBT = EmptyBT 
reflect (NodeBT x lt rt) = NodeBT x (reflect rt) (reflect lt)

minElemOf :: Ord a => BinTree a -> a
minElemOf EmptyBT = error "Empty tree has no minimum element"
minElemOf (NodeBT val left _) = if isLeaf left then val else minElemOf left
  where
    isLeaf EmptyBT = True
    isLeaf _ = False

foldBinTree :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldBinTree _ acc EmptyBT = acc
foldBinTree f acc (NodeBT x left right) = f x (foldBinTree f acc left) (foldBinTree f acc right)