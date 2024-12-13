-- first task

f :: Double -> Double
f p = p 
g :: Double -> Double
g p = p
res_max :: Double -> Double -> Double
res_max p1 p2 = max (f p1) (g p2)

-- second tasks

--res_exp :: Double -> Double -> Double
res_exp p = max (exp p) (f p)



