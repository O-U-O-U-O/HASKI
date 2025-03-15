-- FORTH TASK
import Control.Monad.State
fact' :: State (Int, Int) Int
fact' = do
  (n, sei) <- get 
  if n == 0
    then return sei  
    else do
      put (n - 1, sei * n)  
      fact' 
fact :: Int -> Int
fact n = evalState fact' (n, 1)

main :: IO ()
main = do
  let res = fact 5
  putStrLn $ show res



-- FIFTH TASK
import Control.Monad.State
fibb' :: State (Int, Int, Int) Int
fibb' = do
    (n, curr, past) <- get 
    if n == 0
        then return curr
        else do
            put (n - 1, curr + past, curr)
            fibb'

fibb :: Int -> Int
fibb n = evalState fibb' (n, 0, 1)

main :: IO ()
main = do
    let res = fibb 8
    putStrLn $ show res
