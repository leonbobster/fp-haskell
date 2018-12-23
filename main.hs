import QuickSort (qsort)

main :: IO()
main = do
    let x = qsort([3, 2, 1])
    print x