import Text.ParserCombinators.Parsec


myfilter :: (a -> Bool) -> [a] -> [a] 
myfilter _ [] = []
myfilter f (x:xs)
    | f x == True = x : myfilter f xs
    | otherwise = myfilter f xs

data Pet = Cat | Dog | Fish | Parrot { name :: String } | Snake deriving Show

hello :: Pet -> String
hello x =
    case x of
        Cat -> "meeow"
        Dog -> "woof"
        Fish -> "bubble"
        Parrot name -> "pretty " ++ name
        _ -> "grunt"


maxhelper :: Int -> [Int] -> Int
maxhelper x [] = x
maxhelper x (y:ys) = maxhelper
    (if x > y then x else y) ys


maxfromlist :: [Int] -> Maybe Int
maxfromlist [] = Nothing
maxfromlist (x:xs) = Just (maxhelper x xs)


char :: Char -> [Char] -> Char 
char c s 
    | length s > 0 && f == c = c 
    | otherwise = error ("parse error, unexpected " ++ [f] ++ ", expected " ++ [c])
    where
        f = s !! 0

letter_digit :: Parser Char
letter_digit =
    do x <- letter <|> digit
       return x


bag_bog :: Parser String
bag_bog =
    do xs <- try (string "bag") <|> string "bog"
       return xs 


f :: Num a => a -> a
f x = x * 2
