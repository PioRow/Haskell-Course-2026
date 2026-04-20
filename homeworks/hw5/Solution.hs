import Control.Monad.State (State, runState,get,put, execState)

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

data Tree a = Leaf a | Node (Tree a) (Tree a)
labelTree ::Tree a -> Tree (a,Int)
labelTree t = undefined


main :: IO ()
main = do
    print $ "TASK 1: "
    print $ runProg program
    print $ "TASK 2: "
