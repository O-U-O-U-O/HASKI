--FIRST

data Person = Person {                 -- Jane Russ        Miranda
 name :: String,                         -- Alice       John
 surname :: String,                             --David
 father :: Maybe Person,
 mother :: Maybe Person
} deriving Show

jane = Person "Jane" "Smith" Nothing Nothing
russ = Person "Russ" "Cox" Nothing Nothing
miranda = Person "Miranda" "Lee" Nothing Nothing
alice = Person "Alice" "Cox" (Just russ) (Just jane)
john = Person "John" "Lee" Nothing (Just miranda)
david = Person "David" "Lee" (Just john) (Just alice)

mf :: Person -> Maybe Person
mf (david) = Just alice

main :: IO ()
main = do
   let m = mf (david)
   putStrLn $ show m






--THIRD
sum :: IO ()
sum = 
    readLn >>= \a ->
    readLn >>= \b ->        
    putStrLn $ show (a+b)


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
