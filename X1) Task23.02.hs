--- The First

data LambdaTerm
  = Lam String LambdaTerm -- Abstraction
  | App LambdaTerm LambdaTerm -- Application
  | Var String -- Variable

instance Show LambdaTerm where
  show (Lam x t) = "(λ" ++ x ++ "." ++ show t ++ ")"
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (Var x) = x

main :: IO ()
main = do
  let id' = Lam "x" (Var "x")
  putStrLn $ show id'
