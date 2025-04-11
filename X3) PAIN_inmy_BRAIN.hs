import System.IO
import Data.Char

type Mem = ([Int], Int, [Int])

wtf :: String -> Mem -> IO Mem
wtf [] mem = return mem
wtf (b:bs) (l, c, r) = case b of

    '>' -> wtf bs (c:l, head r, tail r)

    '<' -> wtf bs (init l, last l, c:r)

    '+' -> wtf bs (l, (succ c), r) 

    '-' -> wtf bs (l, (pred c), r) 
    
    '.' -> do
           putChar $ chr c
           hFlush stdout
           wtf bs (l, c, r)

    ',' -> do
           nc <- getChar
           wtf bs (l, ord nc, r)       

    '_' -> wtf bs (l, c, r)

 -- '[' -> if c == 0 then wtf bs (l, c, r)
 --        else if *something from bs* == ']' then *recursion*
 
main :: IO ()
main = do
      let bf = "++++++++++++++++++++++++++++++++++++++++++++++++++\
      \++++++++++++++++++++++++++++++++++++++++++++++++++++++.-------.++++++++++++++++++.\
      \--------.------.+++++++..>++++++++++++++++++++++++++++++++.>+++++++++++++++++++++++\
      \++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.++++++++++.\
      \>++++++++++++++++++++++++++++++++.>>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\
      \+++++++++++++++++++++++++++++++++++++++++++++.++++..++.+++.++.-------------------.+++++++++++.-----\
      \----------------------------------------------------------------------.-.\  
      \>>++ "    
      let memory = ([], 0, replicate 99 0) :: Mem  --there is 30000 size but it's so large
      res <- wtf bf memory
      putStrLn $ show res              

