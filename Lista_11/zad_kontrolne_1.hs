main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")  
	
	
instance Ord a => Prioq ListPrioq a where
    empty = LP []
    isEmpty _ = undefined
    merge _ _ = undefined