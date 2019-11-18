data BTree a = Leaf | Node (BTree a) a (BTree a) deriving (Eq, Ord, Show)
data Array a = Arr (BTree a) Integer deriving (Show, Eq, Ord)

aempty :: Array a
aempty = Arr Leaf 0

asub :: Array a -> Integer -> a
asub (Arr t _) ind = find t ind
    where
        find :: BTree a -> Integer -> a
        find Leaf _ = error "!"
        find (Node l v r) indx
            | (indx == 1) = v
            | (indx `mod` 2 == 0) = find l (indx `div` 2)
            | otherwise = find r (indx `div` 2)


---
asub aempty x = error "!"
asub (Arr (Node l val r) s) ind = val where ind == 1
asub (Arr (Node l val r) s) ind = asub (Arr l s) (ind `div` 2) where (ind `mod` 2) == 0
asub (Arr (Node l val r) s) ind = asub (Arr r s) (ind `div` 2) where (ind `mod` 2) == 1
---


aupdate :: Array a -> Integer -> a -> Array a
aupdate (Arr t size) ind elem = Arr (change t ind elem) size
        where
            change :: BTree a -> Integer -> a -> BTree a
            change Leaf _ _ = error "!"
            change (Node l v r) i e 
                | i == 1 = Node l e r
                | i `mod` 2 == 0 = Node (change l (i `div` 2) e) v r
                | otherwise = Node l v  (change r (i `div` 2) e)
---
aupdate aempty x newv = error "!"
aupdate (Arr (Node l val r) s) ind newv = (Arr (Node l newv r) s) where ind = 1
aupdate (Arr (Node l val r) s) ind newv = let (Arr newleft s2) = (aupdate (Arr l s) (ind `div` 2) newv) in (Arr (Node newleft val r) s) where ind `mod` 2 == 0
aupdate (Arr (Node l val r) s) ind newv = let (Arr newright s2) = (aupdate (Arr r s) (ind `div` 2) newv) in (Arr (Node l val newright) s) where ind `mod` 2 == 0
---



ahiext :: Array a -> a -> Array a
ahiext (Arr t s) x = (Arr (insert t x (s+1)) (s + 1))
    where
        insert :: BTree a -> a -> Integer -> BTree a
        insert Leaf a _ = Node Leaf a Leaf
        insert (Node l val r) x k
            | k `mod` 2 == 0 =
                Node (insert l x (k `div` 2)) val r
            | otherwise =
                Node l val (insert r x (k `div` 2))


ahirem :: Array a -> Array a
ahirem (Arr t size) = Arr (remove t size) (size - 1)
    where
    remove :: BTree a -> Integer -> BTree a
    remove Leaf _ = error "!"
    remove (Node l v r) i
        | i == 1 = Leaf
        | i `mod` 2 == 0 =
        Node (remove l (i `div` 2)) v r
        | otherwise = 
        Node l v (remove r (i `div` 2))