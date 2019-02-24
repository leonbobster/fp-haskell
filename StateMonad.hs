import Prelude hiding (read)
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

data S s a = S { runS :: s -> (a, s) }
-- runS :: S s a -> s -> (a, s)

instance Monad (S s) where
  -- return :: a -> S s a
  return a = S $ \s -> (a, s)
  -- (>>=) :: S s a -> (a -> S s b) -> S s b
  S f >>= k =
    -- f :: s -> (a, s)
    -- k :: a -> S s b
    S $ \s -> let (a, s') = f s
                in runS (k a) s'
    -- k a            :: S s b
    -- runS (k a)     :: s -> (b, s)
    -- runS (k a) s'  :: (b, s) 
                
    
read :: S s s
read = S $ \s -> (s, s)

write :: s -> S s ()
write s = S $ \_ -> ((), s)

foo :: S Int Int
foo = do x <- read
         write (x + 1)
         x <- read
         write (x + 1)
         x <- read
         return x

test :: (Int, Int)
test = runS foo 0

charCounter :: Char -> S String Int
charCounter x = S $ \str -> (length([c | c <- str, c == x]), str)

counter :: S String Int
counter = do x <- charCounter('a')
             y <- charCounter('f')
             z <- charCounter('o')
             return (x + y + z)


testCounter :: (Int, String)
testCounter = runS counter "foobarvoodoo"

    
-- boilerplate code
    
instance Functor (S s) where
  fmap = liftM

instance Applicative (S s) where
  pure  = return
  (<*>) = ap

main :: IO ()
main = do
  print $ testCounter 
