data BinTree a  = EmptyBiNode 
                | BiNode a (BinTree a) (BinTree a) 
                deriving (Eq,Ord,Show,Read) 

bTreeDepth :: BinTree a -> Int
bTreeDepth EmptyBiNode = 0
bTreeDepth (BiNode _ ltTree rtTree) = 1 + max (bTreeDepth ltTree) (bTreeDepth rtTree)

