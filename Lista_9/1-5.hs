{-
WIELOLINIJKOWY
KOMENTARZ
-}
--zadanie1
let f xs = [k | k<- xs, k `mod` (head xs) /= 0]
let l = f [2..]
 
let primes = map head (iterate f l)
 
--zadanie2
primes' = 2:[p | p <- [3..], all (\x -> p `mod` x /= 0) (takeWhile (\x -> x*x <= p) primes')]
 
--zadanie3
let fib = 1:1:(zipWith (+) fib (tail fib))
 
 
--zadanie4
{: insEv : a -> [a] -> [[a]]
insEv x [] = [[x]]
insEv x (ys as (y:ys') = (x:ys):[y:zs | zs<- insEv x ys'] map (y:) (ins Ev x ys`)

iperm2::[a] -> [[a]]
iperm2 [] -> [[]]
iperm2 (x:xs) = [p' | p' <- insEv x p , p <- iperm xs]
:}

{: 
split:: [a] -> [(a, [a])]
split [x] = [(x, [])]
split (x:xs) = (x:xs) : [(y, x:ys) | (y,ys) <- split xs]

sperm2 [] -> [[]]
sperm2 xs = [x:p | (x,r) <- split xs, p <- sperm2 r]
:}

:{
iperm :: [a] -> [[a]]
iperm [] = [[]]
iperm (x:xs) =
    foldr (++) [] (map (insert [] x) (iperm xs))
    where
        insert :: [a] -> a -> [a] -> [[a]]
        insert xs x [] = [xs ++ [x]]
        insert xs x (y:ys) =
            (xs ++ (x : y : ys)) : (insert (xs ++ [y]) x ys)
:}
 
:{
sperm :: [a] -> [[a]]
sperm [] = []
sperm [x] = [[x]]
sperm xs =
    [y:zs | (y,ys) <- select xs, zs <- sperm ys]
    where
        select :: [a] -> [(a, [a])]
        select [x] = [(x, [])]
        select (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- select xs]
 
:}
 
--zadanie5
:{
sublist :: [a] -> [[a]]
sublist [] = [[]]
sublist (x:xs) = let sxs = sublist xs in (map (\ys -> x:ys) sxs) ++ sxs

sublist2 (x:xs) = let sxs = sublist xs in [x:sub | sub <- sxs] ++ sxs
:}
 
--zadanie7
data Tree a = Node (Tree a) a (Tree a) | Leaf
data Set a = Fin (Tree a) | Cofin (Tree a)
:{
   
--tego brakuje
--setUnion, setIntersection :: Ord a => Set a -> Set a -> Set a
--setComplement :: Ord a => Set a -> Set a
 
{- To jednak nie śmiga w ogóle, nawet nie chodzi, właściwie to się czołga

insert :: Ord a => a -> Tree a -> Tree a
insert a Leaf = Node Leaf a Leaf
insert a (Node left val right)
| a < val = Node (insert a left) val right
| otherwise = Node left val (insert a right)
 
buildTree :: Ord a => [a] -> Tree a
buildTree [] = Leaf
buildTree (x:xs) = insert x $ buildTree xs
 
setFromList :: Ord a => [a] -> Set a
setFromList [] = Fin Leaf
setFromList (x:xs) = Fin (buildTree (x:xs))
-}
setFromList2 :: Ord a => [a] -> Set a
setFromList2 [] = Fin Leaf
setFromList2 (x:xs) =  Node ( tFl [e | e <- xs, e<x]) x (tFl [e | e <- xs, e>x])
 
setComplement (Fin t) = Cofin t
setComplement (Cofin t) = FIn t

setIntersection (a as Node (alt av art)) (b as Node (blt bv brt)) = case compare av bv of
	EQ -> Node (blt n alt) av (art 



setEmpty :: Ord a => Set a
setEmpty = Fin Leaf
 
setFull :: Ord a => Set a
setFull = Cofin Leaf
 
 
setMember :: Ord a => a -> Set a -> Bool
setMember _ (Fin Leaf) = False
setMember a (Cofin s) = not (setMember a (Fin s))
setMember a (Fin (Node left val right))
    |  a == val = True
    |  otherwise = setMember a (Fin left) || setMember a (Fin right)
:}
