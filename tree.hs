data Tree = Leaf | Node Int Tree Tree deriving Show

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node x left right) = x + treeSum left + treeSum right

isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x left right) minVal maxVal =
    let leftSorted = isSortedTree left minVal x
        rightSorted = isSortedTree right x maxVal
    in x >= minVal && x < maxVal && leftSorted && rightSorted

addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x + 1) Leaf Leaf) -- rightmost Node
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree

treeToList :: Tree -> [Int]
treeToList Leaf = []
treeToList (Node x left right) = x:treeToList left ++ treeToList right 

treeInsert :: Tree -> Int -> Tree
treeInsert Leaf x = Node x Leaf Leaf
treeInsert (Node x left right) v 
    | v > x = Node x left (treeInsert right v)
    | v < x = Node x (treeInsert left v) right
    | otherwise = Node x left right
