import QuickSort (qsort)
import ValidateCardNumber (isValidCardNumber)

main :: IO()
main = do
    print $ qsort [3, 2, 1]
    print $ isValidCardNumber 4012888888881881