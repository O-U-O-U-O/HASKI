data MozgoVzriv = 
  Lst [Int] | RSt [Int] | Plus [Int] | Minus [Int] | Dot [Int] | Comma [Int] | Rbrack [Int] | Lbrack [Int]
  
so_yee :: MozgoVzriv -> [Int]
so_yee (Lst (x:y:xs)) = (y:xs) 
main :: IO ()
main = do
  let id' = Lst [1,2,3]
  putStrLn $ show id'
