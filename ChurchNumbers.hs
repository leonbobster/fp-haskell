module ChurchNumbers where

type Church a = (a -> a) -> a -> a

zero  = \s z -> z
one   = \s -> s
two   = \s -> s . s
three = \s -> s . s . s

c2i :: Church Int -> Int
c2i x = x (+1) 0

c2s :: Church String -> String
c2s x = x ('*':) ""

add :: Church a -> Church a -> Church a
add x y = \s z -> x s (y s z)

mul :: Church a -> Church a -> Church a
mul x y = x . y