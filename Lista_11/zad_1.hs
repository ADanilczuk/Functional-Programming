ana :: (b -> Maybe (a, b)) -> b -> [a]
ana f st = case f st of
Nothing -> []
Just (v, st') -> v : ana f st'
