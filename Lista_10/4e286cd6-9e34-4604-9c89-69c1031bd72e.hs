data BTree a = Leaf | Node (BTree a) a (BTree a)

dfnum :: BTree a -> BTree Integer
dfnum Leaf = Leaf
dfnum Node (BTree a) a (BTree a) =
    df_help Node (BTree a) a (BTree a) 1
