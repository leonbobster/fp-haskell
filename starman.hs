import System.Random
import Data.Map

check :: String -> String -> Char -> (Bool, String)
check word display c 
    = (c `elem` word, [if c == x then c else y |(x, y) <- zip word display])

turn :: String -> String -> Int -> IO ()
turn word display n =
    do if n == 0
        then putStrLn "You loose"
        else if word == display
            then putStrLn ("The word is `" ++ word ++ "`. You win!")
            else mkguess word display n

mkguess :: String -> String -> Int -> IO ()
mkguess word display n = do
        putStrLn (display)
        putStr " Enter your guess: "
        q <- getLine
        let (correct, display') = check word display (q !! 0)
        let n' = if correct then n else n - 1
        turn word display' n'

rand :: IO Int
rand = randomRIO (1, 2)

starman :: Int -> IO ()
starman n = do
    x <- rand
    let word = fromList [(1, "foo"), (2, "bar")] ! x
    turn word ['-' | x <- word] n


