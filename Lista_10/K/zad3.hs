--gdyby nie bylo typow, to byloby zadanie w stylu wygoogluj paper xD
--kontunuacje 
import Prelude hiding ((^^)) --ukrywa ^^ jako potegowanie

--lit :: (String -> a) -> (String -> a)

lit :: [a] -> ([a] -> t) -> [a] -> t
lit s f stan = f (stan ++ s)

eol :: (String -> a) -> (String -> a)
eol k = \s -> k $ s ++ "\n"

--int :: (String -> a) -> (String -> Integer)

int :: ([Char] -> t) -> [Char] -> a -> t
int k str n = k $ str ++ (show n)

-- skladanie funkcji 
(^^) f g = f . g

float :: (String -> a) -> (String -> float)
float k str n = k $ str ++ (show n)

str :: (String -> a) -> (String -> str)
str k str str2 = k $ str ++ str2

sprintf :: ((a->a) -> String -> t) -> t
sprintf k = k (id) " "

--kontynuacja  ()





