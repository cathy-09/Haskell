import qualified Data.Map as M

data Expr
    = Val Double          
    | Var String          
    | Add Expr Expr       
    | Sub Expr Expr      
    | Mul Expr Expr  
    | Div Expr Expr 
    deriving (Eq)

instance Show Expr where
    show (Val n)     = show n
    show (Var x)     = x
    show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Sub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    show (Mul e1 e2) = show e1 ++ " * " ++ show e2
    show (Div e1 e2) = show e1 ++ " / " ++ show e2

type Env = M.Map String Double

eval :: Env -> Expr -> Double
eval _   (Val n)     = n
eval env (Var x)     = M.findWithDefault 0.0 x env
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Div e1 e2) = eval env e1 / eval env e2

diff :: String -> Expr -> Expr
diff _ (Val _)       = Val 0
diff v (Var x)       = if x == v then Val 1 else Val 0
diff v (Add e1 e2)   = Add (diff v e1) (diff v e2)
diff v (Sub e1 e2)   = Sub (diff v e1) (diff v e2)
diff v (Mul e1 e2)   = Add (Mul (diff v e1) e2) (Mul e1 (diff v e2)) -- (f'g + fg')
diff v (Div e1 e2)   = Div (Sub (Mul (diff v e1) e2) (Mul e1 (diff v e2))) (Mul e2 e2) -- (f'g - fg') / g^2

simplify :: Expr -> Expr
simplify (Add (Val 0) e) = simplify e
simplify (Add e (Val 0)) = simplify e
simplify (Mul (Val 1) e) = simplify e
simplify (Mul e (Val 1)) = simplify e
simplify (Mul (Val 0) _) = Val 0
simplify (Mul _ (Val 0)) = Val 0
simplify (Add e1 e2)     = Add (simplify e1) (simplify e2)
simplify (Mul e1 e2)     = Mul (simplify e1) (simplify e2)
simplify e               = e

main :: IO ()
main = do
    let expr = Add (Mul (Var "x") (Var "x")) (Mul (Val 3) (Var "x"))
    
    putStrLn "Original expression:"
    print expr
    
    let myEnv = M.fromList [("x", 5.0)]
    putStrLn "\nValie by expression x = 5:"
    print (eval myEnv expr)
    
    putStrLn "\nf`(x):"
    let dExpr = diff "x" expr
    print dExpr
    
    putStrLn "\nSimple f`(x):"
    print (simplify dExpr)
