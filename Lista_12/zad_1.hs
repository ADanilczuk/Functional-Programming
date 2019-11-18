import qualified Data.Char as DChr (tolower, isUpper)

class Monad m = StreamTrans m i o | m -> i o where
    readS :: m ( Maybe i )
    emitS :: o -> m ()

tolower = aux 0 where
    aux n = do
        cm <- readS
        if cm == Nothing
            return n
        else
            let (Just c) = cm in
                do
                if P(h.isUpper c) 
                    emitS (P(h.tolower c))
                    aux (n+1)
                else do
                    emitS c
                    aux (n+1)


---- zad 2 ----

instance StreamTrans IO Char Char

readS = do
    cm <- getChar 
    return (if cm == '/n'
         then Nothing
         else Just cm)
    emitS = putChar
                       
    

class Functor t where 
    tmap :: (a->b) -> t a -> t b

newtype LT i o a = LT ([i] -> ([i],[o],a) )

instance Functor (LT i o) where
    tmap f (LT t) = LT \ is -> 
        let (is', os, a)= t is
        in (is', os, f a)


class Applicative t where
    pure :: a -> t a
    <*> :: t (a -> b) -> t a -> t b

instance Applicative (LT i o) where
    pure a = LT (\is -> (is, [], a))
    (LT f) <*> (LT a) = 
        LT (\is -> 
            let (f i, f o, f f) = f is
            in (i', o', a') = a f i
            (i', f o++o', ff a'))

class Monad m where 
    (>>=) :: (a -> m b) -> m a -> m b

LT f >>= g = LT (\is ->
    let (i', o', a') = f is
    let (i'', o'', a'') = unLT (g a), i'
    int (i'', o'++o'', b))

-- instance Monad ( ListTrans i o ) where
-- instance StreamTrans ( ListTrans i o ) i o where     
    
transform :: ListTrans i o a -> [i] -> ([o], a)