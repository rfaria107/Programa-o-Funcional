data BTree a = Empty | Node a (BTree a) (BTree a)
        deriving Show


altura :: BTree a -> Int
altura (Node left a right) = 1 + max (altura left) (altura right)