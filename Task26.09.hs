-- first task

one :: Int -> String
one x = if mod x 2 == 0 
        then "GOOOD" 
        else "WTF"

-- second tasks

two :: Int -> String
two y = if even y == False 
        then "NECHET" 
        else "CHET"

--third task

fibonacha :: Int -> Int
fibonacha 1 = 1
fibonacha n = if n > 0 
              then fibonacha(n - 2) + fibonacha(n - 1)
              else 1


