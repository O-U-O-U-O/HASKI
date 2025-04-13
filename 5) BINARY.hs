-- first task

reverse :: [a] -> [a]
reverse xs = foldl (\a b -> b : a) [] xs -- xs go to []

--forth task

dekart :: [a] -> [a] -> [(a, a)]
dekart xs ys = [(x, y) | x <- xs, y <- ys] -- pair of numbers


--fifth task

type BinaryRelation a = Eq a => [(a, a)]

refl :: Eq a => [a] -> BinaryRelation a -> Bool   -- Рефлексивно ли бинарное отношение rel на множестве m
refl m rel = all (\x -> (x, x) `elem` rel) m      -- проверка рефлексии для каждого элемента х из m

sim :: Eq a => [a] -> BinaryRelation a -> Bool        -- Симметрично ли бинарное отношение rel на множестве m
sim m rel = all (\(x, y) -> (y, x) `elem` rel) m

trans :: Eq a => [a] -> BinaryRelation a -> Bool      -- Транзитивно ли бинарное отношение rel на множестве m
trans m rel = all (\(x, y, z) -> all (\(x, y) -> (y, z) 'elem' rel) rel) m

