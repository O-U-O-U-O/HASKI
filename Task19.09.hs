-- first task

res_max :: (Double -> Double) -> (Double -> Double) -> Double -> Double
res_max f g p = max (f p) (g p)


-- second task

res_exp :: (Double -> Double) -> Double -> Double
res_exp f p = max (exp p) (f p)


-- third task

res_n :: (Double -> Double) -> Double -> Int -> Double
res_n f p n
    | n == 0 = p
    | n < 0 = error "systems-error"
    | otherwise = res_n f (f p) (n-1) -- n-times f communicate with p



