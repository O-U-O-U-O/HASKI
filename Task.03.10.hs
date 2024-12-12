--second tasks

hypo ::(Double, Double) -> (Double, Double) -> Double
hypo (x, y) (w, z) = sqrt ((w - x)^2 + (z - y)^2)        --hypotenuse
perimeter :: [(Double, Double)] -> Double
perimeter [x, y, z] = sum [hypo x y, hypo y z, hypo z x]

--third task

checkAllEq :: Eq a => [a] -> Bool
checkAllEq [_] = True
checkAllEq (x:xs) = all (== x) xs





