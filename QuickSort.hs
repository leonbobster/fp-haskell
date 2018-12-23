module QuickSort (qsort) where

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ larger
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]