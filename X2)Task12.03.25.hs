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
mf bro = 
  case mother bro of  --check bro's mammy 
  Nothing -> Nothing
  Just mamula -> case father mamula of   --check mom's parents 
              Nothing -> Nothing
              Just ded -> Just ded  --ded is found

fm :: Person -> Maybe Person
fm bro = 
   father bro >>= (\_ -> mother bro)
   
main :: IO ()
main = do
   let n = mf (david)
   let m = fm (david)
   putStrLn $ show n
   putStrLn $ show m


--SECOND

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

hp :: Person -> Maybe Person
hp bro = 
  case mother bro of  
  Nothing -> Nothing
  Just mamula -> case father bro of    
              Nothing -> Nothing
              Just papa -> case mother mamula of
                        Nothing -> Nothing
                        Just babkam -> case father mamula of    
                                   Nothing -> Nothing
                                   Just dedm -> case mother papa of
                                             Nothing -> Nothing
                                             Just babkap -> case father papa of
                                                         Nothing -> Nothing
                                                         Just dedp -> Just dedp

main :: IO ()
main = do
   let n = hp david
   putStrLn $ show n



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
