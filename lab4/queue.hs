module Queue
  ( Queue
  , emptyQ  
  , isEmptyQ
  , addQ    
  , remQ     
  ) where 

-- interface (signature, contract)
emptyQ :: Queue a 
isEmptyQ :: Queue a -> Bool 
addQ :: a -> Queue a -> Queue a 
remQ :: Queue a -> (a, Queue a)

-- implementation
newtype Queue a = MkQueue [a] deriving Show

emptyQ = MkQueue [] 
isEmptyQ (MkQueue q) = null q  
addQ x (MkQueue q) = MkQueue (q ++ [x])
remQ (MkQueue (q:qs)) = (q, MkQueue qs)
