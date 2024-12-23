-- first task

reverse :: [a] -> [a]
reverse xs = foldl (\a b -> b : a) [] xs -- xs go to []

--forth task

dekart :: [a] -> [a] -> [(a, a)]
dekart xs ys = [(x, y) | x <- xs, y <- ys] -- pair of numbers




