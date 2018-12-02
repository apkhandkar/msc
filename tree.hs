data Tree a = Node a [Tree a]
            deriving (Eq,Ord,Show,Read)

treeDepth :: Tree a -> Int
treeDepth (Node _ []) = 1
treeDepth (Node _ xs) = 1 + maximum (map treeDepth xs)

