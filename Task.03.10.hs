-- first task

fib :: Int -> Int
fib 1 = 1
fib n = if n > 0 
              then fib(n - 2) + fib(n - 1)
              else error "what"
--fibMod5 :: (Num g) => g -> g
fibMod5 = repeat fib               

-- second tasks


perimetr [(x, y), (z, w), (v, q)] = 
                    let katet = (x + z + w) 
                        catet = (y + z + q)
                        gipot = sqrt(katet + catet)
                    in (katet + catet + gipot )





