data BTree a = Leaf | Node (BTree a) a (BTree a) deriving (Eq, Ord, Show)

df_help :: (Integer, (BTree a)) -> (Integer, (BTree Integer))
df_help (n ,Leaf) = ((n-1), Leaf)
df_help (n, Node (l) a (r)) = let l_new = df_help ((n+1), l) in 
                            let r_new = df_help ( ((fst l_new)+1), r) in
                            ( (fst r_new) , Node (snd l_new) n (snd r_new))
                        

dfnum :: BTree a -> BTree Integer
dfnum Leaf = Leaf
dfnum (Node (l) a (r)) =
    (snd (df_help (1, (Node (l) a (r)))))

dfnum (Node ( Node ( Node Leaf 'a' Leaf ) 'b' Leaf ) 'c' ( Node Leaf 'd' Leaf ))


data BTree a = Leaf | Node (BTree a) a (BTree a) deriving (Eq, Ord, Show)

bf_help :: (Integer, (BTree a)) -> (Integer, (BTree Integer))
bf_help (n ,Leaf) = ((n), Leaf)
bf_help (n, (Node l a r)) = let r_new= (bf_help ((n+1),r)) in (n ,(Node (snd (bf_help ((fst r_new), l))) n (snd r_new)))

bfnum :: BTree a -> BTree Integer
bfnum Leaf = Leaf
bfnum (Node (l) a (r)) = (snd (df_help (1, (Node (l) a (r)))))
    
bf_help (1, Node (Node (Node Leaf 'a' Leaf) 'b' (Node Leaf 'e' Leaf)) 'c' (Node Leaf 'd' Leaf))
