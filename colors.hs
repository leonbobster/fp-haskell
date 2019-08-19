data Bright = Blue | Red deriving (Show, Read)

darkBright :: Bright -> Bool
darkBright Blue = True
darkBright Red = False

lightenBright :: Bright -> Bright
lightenBright Blue = Red
lightenBright Red = Red

data Pastel = Turquoise | Tan deriving (Show, Read)

darkPastel :: Pastel -> Bool
darkPastel Turquoise = True
darkPastel Tan = False

lightenPastel :: Pastel -> Pastel
lightenPastel Turquoise = Tan
lightenPastel Tan = Tan


class Color a where
    dark :: a -> Bool
    lighten :: a -> a

instance Color Bright where
    dark = darkBright
    lighten = lightenBright

instance Color Pastel where
    dark = darkPastel
    lighten = lightenPastel

-- Tuples
mkTup = \x y z -> (\t -> t x y z)
fst' tp = tp (\x y z -> x)
snd' tp = tp (\x y z -> y)




