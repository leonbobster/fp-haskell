import Data.List
import Unsafe.Coerce

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int (Succ n) = nat2int n + 1
nat2int Zero = 0 

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add (Succ m) n = Succ (add m n)
add Zero n = n

natToInteger :: Nat -> Integer
natToInteger = \n -> genericLength [c | c <- show n, c == 'S']
{-- natToInteger = head . m
  where m Zero = [0]
        m (Succ n) = [sum [x | x <- (1: m n)]]
        --}
        
integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Succ (integerToNat (n - 1))
-- integerToNat n = let m = integerToNat (n - 1) in Succ m

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

{-- data Tree = Leaf Integer
          | Node Tree Integer Tree
          
occurs :: Integer -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r)
  = case compare m n of
      LT -> occurs m l
      EQ -> True
      GT -> occurs m r
--}

data Tree = Leaf Integer | Node Tree Tree deriving Show

leaves :: Tree -> Integer
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

balanced :: Tree -> Bool
balanced (Leaf _) =  True
balanced (Node l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r 

balance :: [Integer] -> Tree
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
  where (ys, zs) = splitAt (length xs `div` 2) xs


main :: IO ()
main = do
  print $ nat2int ((Succ Zero) `add` (Succ (Succ Zero)))