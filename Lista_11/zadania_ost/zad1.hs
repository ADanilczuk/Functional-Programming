
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
	
-- tutaj powyższą ideę zamieniłam już po prostu na dwa Either
anaExpr :: (c -> Either (Either a b) (c,c)) -> c -> Expr a b
anaExpr f st = case f st of 
	Left pair -> case pair of 
					Left a -> Number a
					Right b -> Var b
	Right (l, r) -> Plus (anaExpr f l) (anaExpr f r)

--typ cExpr z zajęć
--cExpr :: (b -> c) -> (a -> c) -> (c -> c -> c) -> Expr a b -> c

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

