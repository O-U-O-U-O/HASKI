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



