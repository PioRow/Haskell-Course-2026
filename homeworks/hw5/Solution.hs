import Control.Monad.State (State, runState,get,put, execState,evalState)
import qualified Data.Map as Map

--TASK 1
data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
execInstr :: Instr -> State [Int] ()
execInstr (PUSH x) = do
    stack <- get
    put (x : stack)
execInstr POP = do
    stack <- get
    case stack of
        [] -> return ()
        (_:xs) -> put xs
execInstr DUP = do
    stack <- get
    case stack of
        [] -> return ()
        (x:_) -> put (x : stack)
execInstr SWAP = do
    stack <- get
    case stack of
        (x:y:xs) -> put (y:x:xs)
        _ -> return ()
execInstr ADD = do
    stack <- get
    case stack of
        (x:y:xs) -> put ((y + x) : xs)
        _ -> return ()
execInstr MUL = do
    stack <- get
    case stack of
        (x:y:xs) -> put ((y * x) : xs)
        _ -> return ()
execInstr NEG = do
    stack <- get
    case stack of
        (x:xs) -> put ((-x) : xs)
        _ -> return ()

execProg :: [Instr] -> State [Int] ()
execProg [] = return ()
execProg (i:is) = do
    execInstr i
    execProg is


runProg :: [Instr] -> [Int]
runProg instrs =  (execState (execProg instrs) [])
program = [PUSH 10, DUP, ADD, PUSH 2, MUL, NEG]

data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr   -- bind the value of the expression to the name, return that value
  | Seq  Expr Expr       -- evaluate the left, then the right; return the value of the right

eval :: Expr -> State (Map.Map String Int) Int
eval (Num n) = return n
eval (Var s)=do
    env <- get
    case Map.lookup s env of
        Just v -> return v
        Nothing -> error $ "Variable " ++ s ++ " not found"
eval (Add e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    return (v1 + v2)
eval (Mul e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    return (v1 * v2)
eval (Neg e) = do
    v <- eval e
    return (-v)
eval (Assign s e) = do
    v <- eval e
    env <- get
    let newEnv =Map.insert s v env
    put newEnv
    return v
eval (Seq e1 e2) = do
    v1 <- eval e1
    eval e2

runEval :: Expr -> Int
runEval expr = evalState (eval expr) Map.empty
--TASK 3
editDistM :: String -> String -> Int -> Int -> State (Map.Map (Int, Int) Int) Int
editDistM xs ys i 0 = return i
editDistM xs ys 0 j = return j
editDistM xs ys i j =do
    env <- get
    case Map.lookup (i,j) env of
        Just v-> return v
        Nothing->do
            if xs!!(i-1)==ys!!(j-1)
                then do  
                    res<- editDistM xs ys (i-1) (j-1)
                    let nEnv = Map.insert (i,j) res env
                    put nEnv
                    return res
                else do
                    del <- editDistM xs ys (i-1) j
                    insr <- editDistM xs ys i (j-1)
                    sub <- editDistM xs ys (i-1) (j-1)
                    let mV= minimum [del,insr,sub]
                    let nEnv = Map.insert (i,j) (1+mV) env
                    put nEnv
                    return (1+mV)




editDistance :: String -> String -> Int
editDistance s1 s2 = evalState (editDistM s1 s2 (length s1) (length s2)) Map.empty

main :: IO ()
main = do
    print $ "TASK 1: "
    print $ runProg program
    print $ "TASK 2: "
    print $ runEval (Seq (Assign "x" (Num 5)) (Var "x"))
    print $ runEval (Seq (Assign "x" (Num 5)) (Seq (Assign "y" (Num 10)) (Add (Var "x") (Var "y"))))
    print $ "TASK 3: "
    print $ editDistance "kitten" "sitting"
    print $ editDistance "flaw" "lawn"
    print $ editDistance "intention" "execution"
    print $ editDistance "abc" "abc"


