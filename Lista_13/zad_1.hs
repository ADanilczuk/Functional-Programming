import Prelude hiding ((++), head, tail, length, null, (!!))
import qualified Prelude ((++), head, tail, length, null, (!!))

class List l where
    nil :: l a
    cons :: a -> l a -> l a
    head :: l a -> a
    tail :: l a -> l a
    (++) :: l a -> l a -> l a
    (!!) :: l a -> Int -> a
    toList :: [a] -> l a
    fromList :: l a -> [a]


--- zad 1 ---
instance List [] where 
    nil = []
    cons a ls = a:ls
    head (l:ls) = l
    head nil = error "Empty list"

    tail (l:ls) = ls
    tail nil = error "Empty list"

    (++) nil ls = ls
    (++) ls nil = ls
    (++) (x:xs) ls = x: ( xs ++ ls)

    (!!) (l:ls) 0 = l
    (!!) (l:ls) i =  ls !! (i-1)
    (!!) nil i = error "To short list"

    toList ls = ls
    fromList ls = ls

    
--- zad 2 ---
class List l => SizedList l where
    length :: l a -> Int
    null :: l a -> Bool
    null l = length l == 0


instance List [] => SizedList l where 
    length [] = 0
    length (l:ls) = 1+ (length ls)

    null [] = True
    null (l:ls) = False

--- zad 3 ---
data SL l a = SL { len :: Int, list :: l a }

instance List l => List (SL l) where 
	nil = SL 0 []
	cons a (SL len xs) = SL (len + 1) (a:xs)
	head (SL len []) = error "ERROR"
	head (SL len x:xs) = x
	tail (SL len []) = error "ERROR"
	tail (SL len x:xs) = SL (len - 1) xs
	(++) (SL 0 []) (SL len xs) = SL len xs
	(++) (SL len xs) (SL 0 []) = SL len xs
	(++) (SL len1 x:xs) (SL len2 y:ys) = SL (len1+len2) (x : (xs ++ (y:ys)))
	(!!) (SL 0 []) _ = error
	(!!) (SL len x:xs) 0 = x
	(!!) (SL len x:xs) indx = xs !! (indx-1)
	toList xs = SL (length xs) xs where
		length :: [a] -> Int 
		length [] = 0
		length x:xs = 1 + (length xs)
	fromList (SL len xs) = fromList xs

instance List l => SizedList (SL l) where
	length (SL len _) = len



--- zad 4 ---

infixr 6 :+
data AppList a = 
        Nil 
        Sngl a 
        AppList a :+ AppList a

instance Show => Show (AppList a) where 
    show a = "[" ++ $ show' a ++ "]" where
        show' AppList Nil = ""
        show' AppList Sngl a = show a
        show' AppList (Applist a) :+ Nil = show' $ Applist a
        show' AppList (AppList a) :+ (AppList b) = (show' $ Applist a) ++ "," ++ (show' $ Applist b) 


instance List (AppList a) where 
    nil =Nil
    cons x xs = (Sngl x) :+ xs
    head Nil = error "Empty list"
    head (Sngl a) = a
    head (xs :+ ys) = head xs

    tail Nill = error "Empty list"
    tail (Sngl _) = Nil
    tail (xs :+ ys) = ys

    (++) Nil ys = ys
    (++) (x :+ xs) ys = x :+ (xs ++ ys)
    
    (!!) Nil _ = error "To short"
    (!!) ((Sngl x) :+ _) 0 = x
    (!!) ((Sngl _) :+ xs) n = xs !! (n-1)

    toList [] = Nil
    toList (x:xs) = (Sngl x) :+ toList xs

    fromList Nil = []
    fromList ((Sngl x) :+ xs) = x : fromList xs


instance SizedList (AppList a) where 
    length Nil = 0
    length (Sngl _) = 1
    length (xs :+ ys) = length xs + length ys


--- zad 5 ---
newtype DiffList a = DL ([a]-> [a])

instance List DiffList where 
    nil = Dl id
    cons x (DL xs) = DL (\ys -> x:xs ys)
    head (DL xs) = head (xs [])
    tail (DL xs) = DL(\ys -> tail (xs ys))
    (++) (DL xs) (DL ys) = DL (\zs -> xs (ys zs))
    (!!) (DL xs) n = xs [] !! n
    toList xs = DL (\zs -> xs ++ zs)
    fromList (DL xs) = xs []

instance SizedList DiffList where 
    length (DL xs) = length (xs [])
    null (DL xs) = case xs [] of
        [] -> True
        _ -> False
        
instance Show a => Show (DiffList a) where 
    show (DL xs) = show (xs [])

    
--- zad 6 ---
data RAL a = Empty | Zero (RAL (a,a)) | One a (RAL (a,a))

instance Show a => RAL a where
    show RAL a = "[" ++ $show' a ++ "]" where
        show' (Empty) = ""
        show' (Zero r) = show' r
        show' (One a b) = show a ++ "," ++ $ show' b

instance List RAL where
    nil = Empty
    cons x Empty = One x Empty
    cons x (Zero t) = One x t
    cons x (One y z) = Zero (cons (x,y) z)

    head Empty = error "To short"
    head (One x t) = fst x
    head (Zero t) = fst (head t)

    tail Empty = error "To short"
    tail (One x Empty) = Empty
    tail (One x t) =  Zero t
    tail (Zero t) = tail t

    (++) Empty t = t
    (++) t Empty = t
    (++) t1 t2 = 

    (!!) Empty i = error "To short"
    (!!) t 0 = head t
    (!!) t n = 