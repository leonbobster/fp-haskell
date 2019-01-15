foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = return a
foldLeftM f a (x:xs) = do
  a' <- f a x
  foldLeftM f a' xs
  
foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f b [] = return b
foldRightM f b (x:xs) = do
  b' <- foldRightM f b xs
  f x b'
  
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= \a -> m >>= \b -> return (f b)
-- liftM' f m = do x <- m
--                 return (f x)
  
main :: IO () 
main = do
  liftM (\x -> 0) (return "x") >>= print
  foldLeftM (\a b -> putStr [b] >> return (b : a ++ [b])) [] "haskell" >>= putStrLn 
  foldRightM (\a b -> putStr [a] >> return (a:b)) [] (show [1, 3 .. 10]) >>= putStrLn