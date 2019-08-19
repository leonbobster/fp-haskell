word_to_phrase :: [Char] -> [Char]
word_to_phrase [] = ""
word_to_phrase (x:xs) = [x] ++ " is for " ++ x:xs

glue :: Int -> Int -> [Char]
glue number_of_words index
    | index == 0 = "" 
    | index == number_of_words - 1 = ", and "
    | otherwise = ", "

speller :: [[Char]] -> [Char]
speller [] = ""
speller words = fst (foldl (folder (length words)) ("", 0) words)
    where
        folder l = \acc word ->
            let i = snd acc
            in (fst acc ++ (glue l i) ++ (word_to_phrase word), i + 1)

