-- Lista 11
-- Programowanie Funkcyjne

-- Alicja Danilczuk
-- zadania robiłam razem z Klaudią Osowską


-- zad 1

ana :: (b -> Maybe (a, b)) -> b -> [a]
ana f st = case f st of
    Nothing -> []
    Just (v, st') -> v : ana f st'


zip :: [a] -> [b] -> [(a, b)]
zip xs ys = ana f (xs, ys)
    where
        f :: ([a], [b]) -> Maybe ((a, b), ([a], [b]))
        f ([], _) = Nothing
        f (_, []) = Nothing
        f (x:xs, y:ys) = Just ((x, y), (xs, ys))

iterate :: (a -> a) -> a -> [a]
iterate f start = ana (\st -> Just (st, f st)) start

map :: (a -> b) -> [a] -> [b]
map f xs = ana g xs
    where
        g [] = Nothing
        g (x:xs) = Just (f x, xs)	



cata :: (a -> b -> b) -> b -> [a] -> b
cata f v [] = v
cata f v (x:xs) = f x (cata f v xs)


length :: [a] -> Integer
length xs = cata (\_ res -> 1 + res) 0 xs


filter :: (a -> Bool) -> [a] -> [a]
filter p xs = cata (\elem res -> if p elem then elem : res else res) [] xs


map' :: (a -> b) -> [a] -> [b]
map' f xs = cata (\elem res -> f elem : res) [] xs


data Expr a b =
     Number b
    | Var a
    | Plus (Expr a b) (Expr a b)

-- idea sprzed środowych ćwiczeń
data Either3 a b c =
      Left' a
    | Middle' b
    | Right' c

anaExpr :: (c -> Either3 a b (c, c)) -> c -> Expr a b
anaExpr f st = case f st of
    Left' num -> Number num
    Middle' var -> Var var
    Right' (l, r) -> Plus (anaExpr f l) (anaExpr f r)
	
-- tutaj powyższą ideę zamieniłam na po prostu dwa Either
anaExpr :: (c -> Either (Either a b) (c,c)) -> c -> Expr a b
anaExpr f st = case f st of 
	Left pair -> case pair of 
					Left a -> Number a
					Right b -> Var b
	Right (l, r) -> Plus (anaExpr f l) (anaExpr f r)

--typ cExpr z zajęć
--cataExpr :: (b -> c) -> (a -> c) -> (c -> c -> c) -> Expr a b -> c

cataExpr :: (a -> b, b -> b -> b) -> Expr a b -> b
cataExpr f@(env, (·)) expr = case expr of
    Number n -> n
    Var x -> env x
    Plus a b -> (cataExpr f a) · (cataExpr f b)

eval :: Num b => Eq a => [(a , b)] -> Expr a b -> b
eval env expr = cataExpr (lookup, (+)) expr
    where
        lookup var = snd $ head $ Main.filter ((== var) . fst) env



eval_p :: Num b => (a -> b) -> Expr a b -> b
eval_p env = cExpr id env (+) 



-----------
-- zad 2

{-# LANGUAGE KindSignatures, MultiParamTypeClasses, FlexibleInstances #-}

import Data.List (unfoldr)
import Data.Bool (bool)

(><) :: (a -> b) -> (a -> c) -> a -> (b, c)
(f >< g) x = (f x, g x)

warbler :: (a -> a -> b) -> a -> b
warbler f x = f x x

class Ord a => Prioq (t :: * -> *) (a :: *) where
    empty       :: t a
    isEmpty     :: t a -> Bool
    single      :: a -> t a
    insert      :: a -> t a -> t a
    merge       :: t a -> t a -> t a
    extractMin  :: t a -> (a, t a)
    findMin     :: t a -> a
    deleteMin   :: t a -> t a
    fromList    :: [a] -> t a
    toList      :: t a -> [a]
    
    insert = merge . single
    single = flip insert empty
    extractMin = findMin >< deleteMin
    findMin = fst . extractMin
    deleteMin = snd . extractMin
    fromList = foldr insert empty
    toList = 
        unfoldr . warbler $ bool (Just . extractMin) (const Nothing) . isEmpty

newtype ListPrioq a = LP { unLP :: [a] } deriving Show

instance Ord a => Prioq ListPrioq a where
    empty = LP { unLP = [] }
    isEmpty xs = (unLP xs) == []
    merge q1 q2 = LP $ merge' (unLP q1) (unLP q2)
        where
            merge' :: Ord a => [a] -> [a] -> [a]
            merge' [] q = q
            merge' q [] = q
            merge' q1@(x:xs) q2@(y:ys)
                | x < y = x : merge' xs q2
                | otherwise = y : merge' q1 ys
    single a = LP [a]
    insert a q = LP $ insert' a (unLP q)
        where
            insert' :: Ord a => a -> [a] -> [a]
            insert' a [] = [a]
            insert' a q@(x:xs)
                | a < x = a : q
                | otherwise = x : insert' a xs
    extractMin (LP []) = error "extractMin: empty queue!"
    extractMin (LP (q:qs)) = (q, LP qs)

	
-----------
-- zad 3

-- Tutaj dużo nie ma, ale przynajmniej te łatwiejsze części

{-# LANGUAGE RankNTypes #-}

newtype Church = Church (forall a. (a -> a) -> (a -> a))

zero :: Church
zero = Church (\f x -> x)

isZero :: Church -> Bool
isZero (Church n) = n (\x -> False) True

successor :: Church -> Church
successor (Church n) = Church (\f x -> f (n f x))

-- definicja w internecie wprawdzie jest ale dla mnie 
-- jakoś niezbyt jasna więc tego nie mam
predecessor :: Church -> Church

add :: Church -> Church -> Church
add (Church m) (Church n) = Church (\f x -> m f (n f x))

multiply :: Church -> Church -> Church
multiply (Church m) (Church n) = Church (\f -> m (n f))

-- w sumie tak jak z poprzednikiem
substract :: Church -> Church -> Church

intToChurch :: Integer -> Church
intToChurch 0 = zero
intToChurch n = successor $ intToChurch $ n -1

churchToInt :: Church -> Integer
churchToInt n
    | isZero n = 0
    | otherwise = 1 + (churchToInt $ predecessor n)

instance Eq Church where
    n == m
        | isZero n = isZero m
        | otherwise = predecessor n == predecessor m

instance Show Church where
    show n = show $ churchToInt n

instance Ord Church where
    compare n m = compare (churchToInt n) (churchToInt m)
    n <= m = (churchToInt n) <= (churchToInt m)

	
------------
-- zad 4

import System.IO

main = play [5,4,3,2,1]

play :: [Int] -> IO ()
play board = do
    (newBoard, winner) <- takeTurns board
    case winner of
        0 -> play newBoard
        1 -> do putStrLn "You won!"
                return ()
        2 -> do putStrLn "Computer won!"
                return ()

takeTurns :: [Int] -> IO ([Int],Int)
takeTurns board = do
    newBoard <- playerTurn board
    if sum newBoard == 0 then
        return (newBoard, 1)
    else do
        newBoard <- computerTurn newBoard    
        case sum newBoard of
            0 -> return (newBoard, 2)
            _ -> return (newBoard, 0)

playerTurn :: [Int] -> IO [Int]
playerTurn board = do
    putStrLn "\nYour turn"
    displayBoard board
    row   <- getRow board
    count <- getInt "How many stars? " 1 (board!!row)
    return $ (take row board) ++ [board!!row - count] ++ drop (row+1) board

computerTurn :: [Int] -> IO [Int]
computerTurn board =
    let (row, count) = makeTurn board 0 in
    return $ take row board ++ [board!!row - count] ++ drop (row+1) board

makeTurn :: [Int] -> Int -> (Int, Int)
makeTurn board row_num =
    let how_many = board!!row_num in
    if how_many == 0 then
        makeTurn board (row_num +1)
    else
        (row_num, 1)


displayBoard :: [Int] -> IO ()
displayBoard [] = return ()
displayBoard board = do
    putStrLn $ show (length board) ++ " : " ++ replicate (last board) '*'
    displayBoard (init board)

getRow :: [Int] -> IO Int
getRow board = do
    row <- getInt "Which row? " 1 (length board)
    if board!!(row-1) == 0
      then do putStrLn "That row is empty!"
              getRow board
      else return (row-1)

getInt msg min max = do
    putStrLn msg
    input <- getLine
    let parsed = reads input :: [(Int,String)] --zamiana stringa na int i resztę stringa
    if null parsed
      then badNumber "Not a number"
      else testNumber (fst (head parsed))
    where
        badNumber error = do putStrLn error
                             getInt msg min max
        testNumber number
            | number < min = badNumber "Too small"
            | number > max = badNumber "Too big"
            | otherwise    = return number
