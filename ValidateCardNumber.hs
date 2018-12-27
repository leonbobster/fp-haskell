module ValidateCardNumber where

import Prelude hiding (reverse)

eval :: [Integer] -> Integer
eval xs = foldl (\x y -> y + (10 * x)) 0 xs

toDigits :: Integer -> [Integer]
toDigits n
    | n >= 0 && n < 10 = [n]
    | otherwise = toDigits q ++ [m]
    where 
        q = n `div` 10
        m = n `mod` 10
    
reverse :: [Integer] -> [Integer]
reverse [x] = [x]
reverse (x:xs) = reverse xs ++ [x]

doubleSecond :: [Integer] -> [Integer]
doubleSecond xs = [(xs !! i) * (if i `mod` 2 == 0 then 1 else 2) | i <- [0..n]]
    where
        n = length xs - 1
    
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumDigits xs + z
    where
        z = if x >= 0 && x < 10 then x else sumDigits $ toDigits x
    
isValidCardNumber :: Integer -> Bool
isValidCardNumber x = (flip mod 10 $ sumDigits $ doubleSecond $ reverse $ toDigits x) == 0