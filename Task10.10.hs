--first task

mapl :: (Int -> Int) -> [Int] -> [Int]
mapl f xs = foldl (\a b -> a ++ [f b]) [] xs      -- a - list, b - elem from xs; every time a get result (f b)
mapr :: (Int -> Int) -> [Int] -> [Int]
mapr f xs = foldr (\a b -> [f a] ++ b) [] xs      -- reverse


--forth work

inst :: String -> Bool
inst str = str `elem` ["inc", "dec", "double", "sqrt", "halveIfPositive"]  --predicat
cleaner :: [String] -> [String]
cleaner xs = filter inst xs