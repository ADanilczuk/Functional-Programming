data BTree a = Leaf | Node (BTree a) a (BTree a) deriving (Eq, Ord, Show)
data Array a = Arr (BTree a) Integer deriving (Eq, Ord, Show)

aempty :: Array a
aempty = Arr Leaf 0

asub :: Array a -> Integer -> a
asub (Arr (Node l a r) m) n =
    if n == 1 
        then a
        else if (n `mod` 2) == 0 
            then asub (Arr l m) (n `div` 2)
            else asub (Arr r m) (n `div` 2)



update_h :: BTree a -> Integer -> a -> BTree a
update_h Leaf 0 new_a= Node Leaf new_a Leaf
update_h (Node l a r) n new_a =
    if n == 1 
        then Node l new_a r
        else if (n `mod` 2) == 0 
            then Node (update_h l (n `div` 2) new_a) a r
            else Node l a (update_h r (n `div` 2) new_a)
            
aupdate :: Array a -> Integer -> a -> Array a
aupdate Leaf 0 new_a= Arr (Node Leaf new_a Lea) 1
aupdate (Arr (Node l a r) m) n new_a =
    if n == 1 
        then Arr (Node l new_a r) m
        else if (n `mod` 2) == 0 
            then Arr (Node (update_h l (n `div` 2) new_a) a r) m
            else Arr (Node l a (update_h r (n `div` 2) new_a)) m

ext_help :: Array a -> a -> Integer -> Array a
ext_help Leaf 1 new_a = (Node Leaf new_a Leaf, 1)
ext_help ((Node l a r), m) n new_a = 
    if n == 2 
        then (Node ((Node Leaf new_a Leaf), m+1) a r) , m+1)
        else if n == 3 
            then (Node l a ((Node Leaf new_a Leaf),m+1), m+1)
        else if (n `mod` 2) == 0 
            then (Node (ext_help l (n `div` 2) new_a) a r, m=1)
            else (Node l a (ext_help r (n `div` 2) new_a), m+1) 

ahiext :: Array a -> a -> Array a
ahiext Leaf a = (Node Leaf new_a Leaf, 1)
ahiext ((Node l a r), m) new_a = 
        ext_help ((Node l a r),m) m+1 new_a

rem_help :: Array a -> n -> Array a
rem_help (Node Leaf a Leaf, 1) 1 = (Leaf, 0)
rem_help ((Node l a r), m) n=
    if n == 2 
        then (Node (Leaf, m-1) a r) , m-1)
        else if n == 3 
            then (Node l a (Leaf, m-1), m-1)
        else if (n `mod` 2) == 0 
            then (Node (rem_help l (n `div` 2)) a r, m-1)
            else (Node l a (rem_help r (n `div` 2)), m-1) 

ahirem :: Array a -> Array a
ahirem  (Node Leaf a Leaf, 1) = (Leaf, 0)
ahirem ((Node l a r), m) =  rem_help ((Node l a r), m) m