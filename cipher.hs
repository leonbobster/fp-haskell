import Data.Char

-- Test
-- quickCheck ((\n -> (\s -> ((decipher n (cipher n s)) == s))) :: Int -> [Char] -> Bool)

shouldcipher :: Char -> Bool
shouldcipher c = isLetter(c) && isAscii(c)

cipherchar :: Int -> Char -> Char
cipherchar shift c
    | shouldcipher c    = chr(ord(c) + shift)
    | otherwise         = c

cipher :: Int -> [Char] -> [Char]
cipher shift plaintext = map (bettercipherchar shift) plaintext

decipher :: Int -> [Char] -> [Char]
decipher shift ciphertext = cipher (-shift) ciphertext

-- should we wrap around the alphabet?
wraparound shift c
    | isLower(c) && ord(c) + shift > ord 'z' = True
    | isUpper(c) && ord(c) + shift > ord 'Z' = True
    | otherwise = False

bettercipherchar :: Int -> Char -> Char
bettercipherchar shift c
    | shouldcipher c = chr(ord(c) + adjustshift)
    | otherwise = c
    where adjustshift = let shift' = shift `mod` 26
                        in if (wraparound shift' c)
                            then shift' - 26
                            else shift'