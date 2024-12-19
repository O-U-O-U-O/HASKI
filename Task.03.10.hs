--second tasks

hypo ::(Double, Double) -> (Double, Double) -> Double
hypo (x, y) (w, z) = sqrt ((w - x)^2 + (z - y)^2)        
perimeter :: [(Double, Double)] -> Double
perimeter [x, y, z] = sum [hypo x y, hypo y z, hypo z x]



--third task

checkAllEq :: Eq a => [a] -> Bool
checkAllEq [_] = True
checkAllEq (x:xs) = all (== x) xs



--fifth task

computeProgram :: [String] -> Double -> Double
computeProgram [] p = p
computeProgram (x:xs) p                          --with all elements
    | x == "inc" = computeProgram xs (p + 1)
    | x == "dec" = computeProgram xs (p - 1)
    | x == "double" = computeProgram xs (p * 2)
    | x == "sqrt" = computeProgram xs (sqrt p)
    | x == "halveIfPositive" = computeProgram xs (if p < 0 then p else p / 2)
    | otherwise = error "systems-errror"





