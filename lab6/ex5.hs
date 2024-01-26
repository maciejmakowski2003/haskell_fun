foldM' :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldM' _ acc [] = return acc
foldM' f acc (x:xs) = do
  acc' <- f acc x
  foldM' f acc' xs

forM' :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM' [] _ = return []
forM' (x:xs) f = do
  result <- f x
  rest <- forM' xs f
  return (result : rest)

mapM' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM' _ [] = return []
mapM' f (x:xs) = do
  result <- f x
  rest <- mapM' f xs
  return (result : rest)

filterM' :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x:xs) = do
  condition <- p x
  rest <- filterM' p xs
  if condition
    then return (x : rest)
    else return rest

zipWithM' :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM' _ [] _ = return []
zipWithM' _ _ [] = return []
zipWithM' f (x:xs) (y:ys) = do
  result <- f x y
  rest <- zipWithM' f xs ys
  return (result : rest)

liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f ma mb = do
  a <- ma
  b <- mb
  return (f a b)

guard :: MonadPlus m => Bool -> m ()
guard True  = return ()
guard False = mzero