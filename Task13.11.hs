-- first task

newtype Stack = Stack [Int]

emptyStack :: Stack               -- Сконструировать пустой стек
emptyStack = Stack []

push :: Stack -> Int -> Stack     -- Добавить элемент на вершину стека, Возвращается новое состояние стека
push (Stack []) x = Stack [x]     -- 1 элемент
push (Stack xs) x = Stack (x:xs)  -- добавляется вершина в начало списка

pop :: Stack -> (Int, Stack)            -- Взять элемент с вершины стека, Возвращается кортеж (верхний_элемент, новое_состояние_стека)
pop (Stack []) = error "dla filosofov"  -- у пустого либо нет вершин, либо все - вершины
pop (Stack (x:xs)) = (x, Stack xs)      -- вершина и оставшийся список








