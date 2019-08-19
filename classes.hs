{-# LANGUAGE FlexibleInstances #-}

class Foo a where
    bar :: a -> String

instance Foo Int where
    bar n = "funny number " ++ (show n)

instance Foo [Char] where
    bar xs = "funny string " ++ xs