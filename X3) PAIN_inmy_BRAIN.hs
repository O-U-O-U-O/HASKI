data MozgoVzriv = 
  Lst [Int] | RSt [Int] | Plus [Int] | Minus [Int] | Dot [Int] | Comma [Int] | Rbrack [Int] | Lbrack [Int]
  
so_yee :: MozgoVzriv -> [Int]
so_yee (Lst (x:y:xs) = (y:xs)
so_yee (Rst (xs:y:x) = (xs:y)
so_yee (Plus (x:xs) = (x+1:xs)
so_yee (Minus (x:xs) = (x-1:xs)
so_yee (Dot (x:xs) = (x-1:xs)
main :: IO ()
main = do
  let id' = Lst [1,2,3]
  putStrLn $ show id'
