import System.IO

type Mem ([Int], Int, [Int])

wtf :: String -> Mem -> IO Mem
wtf [] memory = return memory
wtf (b:bs) (l, c, r)= case b of

    '>' -> wtf bs (nl, nc, nr)
           where nl = l ++ [c]
                nc = head r 
                nr = tail r

    '<' -> wtf bs (nl, nc, nr)
           where nl = init l
                nc = last l
                nr = (c:r)

    '+' -> wtf bs (l, (succ c), r) 

    '-' -> wtf bs (l, (pred c), r) 
    
    '.' -> do
           putChar c
           wtf bs (l, c, r)
    ',' -> do
           nc <- getChar
           wtf bs (l, nc, r)      

    '_' -> wtf bs (l, c, r)


main :: IO ()
main = do
  let bf = "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.-------.++++++++++++++++++.--------.------.+++++++..>++++++++++++++++++++++++++++++++.>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.++++++++++.>++++++++++++++++++++++++++++++++.>>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.++++..++.+++.++.-------------------.+++++++++++.\
\>>++"
  let memory = ([], 0, replicate 30000 0) :: Mem 
  res <- wtf bf memory
  putStrLn $ show res              

