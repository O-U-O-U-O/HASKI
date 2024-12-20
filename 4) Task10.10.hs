--first task

mapl :: (a -> a) -> [a] -> [a]
mapl f xs = foldl (\a b -> a ++ [f b]) [] xs      -- a - list, b - elem from xs; every time a get result (f b)
mapr :: (a -> a) -> [a] -> [a]
mapr f xs = foldr (\a b -> [f a] ++ b) [] xs      -- reverse


--forth work

inst :: String -> Bool
inst str = str `elem` ["inc", "dec", "double", "sqrt", "halveIfPositive"]  --predicat
cleaner :: [String] -> [String]
cleaner xs = filter inst xs
