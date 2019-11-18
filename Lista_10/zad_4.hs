data Color = Red | Black
data RBTree a = RBNode Color ( Tree a ) a ( Tree a ) | RBLeaf