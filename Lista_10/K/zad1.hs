data BTree a = Leaf | Node (BTree a) a (BTree a) deriving (Eq, Ord, Show)


dfhelp :: (Integer, BTree a) -> (Integer, BTree Integer)
dfhelp (n, Leaf) = (n-1, Leaf)
dfhelp (n, Node l a r) = let lnew = dfhelp (n + 1, l) in let rnew = dfhelp ((fst lnew)+1, r) in (fst rnew, Node (snd lnew) n (snd rnew))

dfnum ::  BTree a -> BTree Integer
dfnum Leaf = Leaf
dfnum (Node l a r) = (snd (dfhelp (1, Node l a r)))

---------






jak ponumerowac las (dfs) ?
1) k i 
  'a'
b     c 

i reszta lasu f

zeby je ponumerowac dajemy numer a(k) i potem numerujemy b:c:f

2)k, [] -> []
3) k, lisc:f
    ponumeruj (k, f), dostaniemy f'
    i zwroc lisc:f'

--------

(Node ( Node ( Node Leaf 'a' Leaf ) 'b' Leaf ) 'c' ( Node Leaf 'd' Leaf ))
(Node (Node (Node Leaf 'a' Leaf) 'b' (Node Leaf 'c' (Node Leaf 'd' Leaf))) 'e' (Node Leaf 'f' (Node Leaf 'g' Leaf)))


------ jak numerowac las (bfs)
1) KOLEJKA FIFO
k, 
   x
t1   t2
i reszta lasu f

f t1 t2
ponumeruj i dolacz z prawej strony do f'



------------------------------

------
bfn:: ([Integer], BTree a) -> ([Integer], Btree a)
bfn (xs, Leaf) = (xs, Leaf)
bfn (x:xs (Node (t _ rt))) = (x+1:xs'', (Node (tn) x rt')


data Queue a = Queue { 
  inbox :: [a], 
  outbox :: [a] 
} deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push e (Queue inb out) = Queue (e:inb) out

pop :: Queue a -> (a, Queue a)
pop q@(Queue inb []) = pop $ Queue [] (reverse inb)
pop (Queue inb outb) = (head outb, Queue inb (tail outb))


bfnum :: BTree a -> BTree Integer
bfnum Leaf = Leaf
bfnum (Node l a r) = (snd (bfhelp (1, Node l a r)))

bfhelp :: (Integer, BTree a) -> (Integer, BTree Integer)
bfhelp (n, Leaf) = 


