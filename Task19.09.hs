-- first task

fo :: Double -> Double
fo x = x 
go :: Double -> Double
go x = x 
res_max:: Double -> Double -> Double
res_max x y = max (fo x) (go y)

-- second tasks

res_exp :: Double -> Double -> Double
res_exp p f = max (exp p) (fo f)

--third task

res_n :: Double -> Double -> Double
res_n p n = if n == 0 then (fo p) else (if n < 0 then "error" else (fo p) * n)

