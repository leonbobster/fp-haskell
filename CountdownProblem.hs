data Op = Add | Sub | Mul | Div deriving Show
data Expr = Val Int | App Op Expr Expr deriving Show

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y 
valid Sub x y = x > y
valid Mul x y = x <= y && x /= 1 && y /= 1
valid Div x y = x `mod` y == 0

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l
                                , y <- eval r
                                , valid o x y]

-- Return a list of all the values in an expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- Decide if an expression is a solution for a given list of source numbers and a target number
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n] 

-- Combinatorial functions

choices' :: [a] -> [[a]]
choices' [] = [[]]
choices' (x:xs) = ys ++ zs  
  where
    ys = choices' xs
    zs = concat [interleave x y | y <- ys]

interleave' :: a -> [a] -> [[a]]
interleave' x [] = [[x]]
interleave' x ys = [take i ys ++ [x] ++ drop i ys | i <- [0 .. length ys]]

-- Returns all subsequences of a list, which are given by all possible
-- combinations of excluding or including each element of the list
subs                          :: [a] -> [[a]]
subs []                       =  [[]]
subs (x:xs)                   =  yss ++ map (x:) yss where yss = subs xs

interleave                    :: a -> [a] -> [[a]]
interleave x []               =  [[x]]
interleave x (y:ys)           =  (x:y:ys) : map (y:) (interleave x ys)

-- Returns all permutations of a list, which are given by all possible
-- reorderings of the elements
perms                         :: [a] -> [[a]]
perms []                      =  [[]]
perms (x:xs)                  =  concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs, zs <- perms ys]

-- Return a list of all possible ways of splitting a list into two non-empty parts
split :: [a] -> [([a], [a])]
split xs = [(take i xs, drop i xs) | i <- [1 .. length xs - 1]]


-- Return a list of all possible expressions whose values are precisely a given list of numbers
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns
              , l <- exprs ls
              , r <- exprs rs
              , e <- combine l r]

-- Combine two expressions using each operator
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

-- Return a list of all possible expressions that solve an instance of the countdown problem
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns
                    , e <- exprs ns'
                    , eval e == [n]]
                    
                    
type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls,  rs) <- split ns
                  , lx <- results ls
                  , ry <- results rs
                  , res <- combine' lx ry]
                  
combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) =
  [(App o l r, apply o x y)
    | o <- [Add, Sub, Mul, Div]
    , valid o x y]
    
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  [e | ns' <- choices ns
     , (e, m) <- results ns'
     , m == n]

main :: IO ()
main = do
  print $ length $ solutions' [1, 3, 7, 10, 25, 50] 765