data BinTree a =
    Nil |
    Node {
          left :: BinTree a,
          right :: BinTree a,
          value :: a,
          count :: Int
    }
-- для более удобного вывода
instance Show a => Show (BinTree a) where
        show = show0 0 where
              show0 _ Nil = "Nil"
              show0 lvl Node{left=l, right=r, value=v, count=cnt} =
                   "Node (v = " ++ show v ++ ")\n" ++
                    replicate lvl '\t' ++ "l=" ++ show0 (lvl+1) l ++ "\n" ++
                    replicate lvl '\t' ++ "r=" ++ show0 (lvl+1) r ++ "\n"

-- first task

insert :: Ord a => BinTree a -> a -> BinTree a
insert Nil x = Node Nil Nil x 1                    --if nil, count++
insert (Node l r v cnt) x
    | x == v = Node l r v (cnt + 1)
    | x < v  = Node (insert l x) r v cnt    --go to left tree
    | x > v  = Node l (insert r x) v cnt

fromList :: Ord a => [a] -> BinTree a
fromList = foldl insert Nil


-- second task

findMin :: BinTree a -> Maybe a
findMin Nil = Nothing
findMin (Node Nil _ v _) = Just v  --if left null
findMin (Node l _ _ _) = findMin l --in left

findMax :: BinTree a -> Maybe a
findMax Nil = Nothing
findMax (Node _ Nil v _) = Just v  --if right null
findMax (Node _ r _ _) = findMin r --in left
