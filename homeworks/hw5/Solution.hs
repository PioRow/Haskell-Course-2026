import Control.Monad.State (State, runState,get,put, execState,evalState, StateT (runStateT))
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT, evalStateT, modify)
import Text.Read (readMaybe, lift)

import Data.List (elemIndex)
import Data.Graph (path)
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
--TASK 4
type ID=Int
data Location=
     Start ID
    |Decision [String] [ID] ID
    | Obstacle Int ID
    | Treasure Int ID
    | Trap Int ID
    | Exit ID
    | Path ID
    | Event ID

instance Show Location where
    show (Start id) = " Start of adventure"++"\n"
    show (Decision options _ id) = " Decisions "++ show options++"\n"
    show (Obstacle difficulty id) =  " Obstacle " ++ "losing "++show difficulty ++ "energy "++"\n"
    show (Treasure value id) = "Treasure " ++ show value ++ " points gained "++"\n"
    show (Trap damage id) = "Trap " ++ show damage ++ " points lost "++"\n"
    show (Exit id) = "  Game Finished !!!"++"\n"
    show (Path id) = " nothing interesting here, just a path to "++"\n"
    show (Event id) = "This event Occured->"

data Node=Node{
    current::Location,
    paths::[ID]
}
nodeIO=Node (Event (-1)) []

--type AdventureGame a = StateT GameState IO a
data GameState=GameState{
    currentNode::Node,
    nodes::Map.Map ID Node,
    energy::Int,
    points::Int,
    pathHistory::[Node]
}
type AdventureGame a = StateT GameState IO a

getDiceRoll :: IO Int
getDiceRoll = do
    print$ " Enter your dice roll (1-6): "
    input <- getLine
    case readMaybe input of
        Just n | n >= 1 && n <= 6 -> return n
        _ -> putStrLn "Invalid roll! Try again." >> getDiceRoll


getPlayerChoice  :: [String] -> IO String
getPlayerChoice options = do
    putStrLn "Choose an option:"
    mapM_ (\(i, opt) -> putStrLn $ show i ++ ": " ++ opt) (zip [1..] options)
    choice <- getLine
    case readMaybe choice of
        Just n | n >= 1 && n <= length options -> return (options !! (n - 1))
        _ -> putStrLn "Invalid choice! Try again." >> getPlayerChoice options
makeDecision :: [String] -> AdventureGame String
makeDecision options = do
        liftIO $ getPlayerChoice options


movePlayer :: Int -> AdventureGame Int
movePlayer 0 = return 0
movePlayer steps = do
    gameState <- get
    let currentLoc = currentNode gameState
    modify $ \s -> s { energy = energy s - 5 }
    case current (currentLoc) of
        Exit _ -> return 0
        Decision options nextIds _ -> do
            choice <-makeDecision options
            let chosenIndex = case elemIndex choice options of
                    Just idx -> idx
                    Nothing -> 0-- Wont happen, already validated
            let nextId = nextIds !! chosenIndex
            modify $ \s -> s { pathHistory = pathHistory s ++ [nodeIO] }
            modify $ \s -> s { pathHistory = pathHistory s ++ [currentLoc] }
            case Map.lookup nextId (nodes gameState) of
                Just nextNode -> do
                    modify $ \s -> s { currentNode = nextNode }
                    movePlayer (steps - 1)
                Nothing -> do 
                    liftIO $ putStrLn "OH NO! someone can't create a good map. just ctrl +C"
                    return 0
        _ -> do
            let nextId = case paths currentLoc of
                    (nid:_) -> nid
                    [] -> -1 -- No paths, should not happen
            case Map.lookup nextId (nodes gameState) of
                Just nextNode -> do
                    modify $ \s -> s { currentNode = nextNode }
                    movePlayer (steps - 1)
                Nothing -> do 
                    liftIO $ putStrLn "OH NO! someone can't create a good map. just ctrl +C"
                    return 0

            
handleLocation :: AdventureGame Bool
handleLocation = do
    gameState <- get
    let loc = currentNode gameState
    modify $ \s -> s { pathHistory = pathHistory s ++ [nodeIO] }
    modify $ \s -> s { pathHistory = pathHistory s ++ [loc] }
    case current loc of
        Start _ -> return False
        Path _ -> return False
        Obstacle difficulty _ -> do
            modify $ \s -> s { energy = energy s - difficulty }
            return False
        Treasure value _ -> do
            modify $ \s -> s { points = points s + value }
            return False
        Trap damage _ -> do
            modify $ \s -> s { points = points s - damage }
            return False
        Decision options nextIds _ -> do
            return False
        Exit _ -> return True



displayGameState :: GameState -> IO ()
displayGameState gameState = do
    putStrLn $ "Current Location: " ++ show (current (currentNode gameState))
    putStrLn $ "Energy: " ++ show (energy gameState)
    putStrLn $ "Points: " ++ show (points gameState)
    putStrLn $ "Journey History: " ++ concatMap (\s -> show (current s) ++ ", ") (pathHistory gameState)

playTurn :: AdventureGame Bool
playTurn = do
    roll <- liftIO getDiceRoll
    liftIO $ putStrLn $ "You rolled a " ++ show roll ++ "!"
    movePlayer roll
    res<-handleLocation
    gameState <- get
    liftIO $ displayGameState gameState
    return res

playGame :: AdventureGame ()
playGame = do
    turnRes <- playTurn
    if turnRes
        then liftIO $ putStrLn "Congratulations! You've completed the adventure!"
        else playGame

----- DUMMY DATA
n0 = Node (Start 0) [1]
n1 = Node (Path 1) [2, 3]

n2 = Node (Obstacle 30 2) [4]
n4 = Node (Trap 50 4) [5]
n5 = Node (Decision ["Push through", "Retreat"] [8, 1] 5) [8, 1]

n3 = Node (Path 3) [6]
n6 = Node (Treasure 100 6) [7]
n7 = Node (Decision ["To the Core", "Side Path","Impending Doom"] [8,9,12] 7) [8, 9,12]

n8 = Node (Obstacle 10 8) [10]
n9 = Node (Path 9) [10]
n10 = Node (Treasure 500 10) [11]
n11 = Node (Exit 11) []

n12 = Node (Trap 5 12) [13]
n13 = Node (Path 13) [12] --

gameNodes :: Map.Map ID Node
gameNodes = Map.fromList [
    (0,n0), (1,n1), (2,n2), (3,n3), (4,n4), (5,n5),
    (6,n6), (7,n7), (8,n8), (9,n9), (10,n10), (11,n11),
    (12,n12), (13,n13)
 ]

initialState :: GameState
initialState = GameState {
    currentNode = n0,
    nodes = gameNodes,
    energy = 100,
    points = 0,
    pathHistory = []
}
----- DUMMY DATA END





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
    liftIO $ evalStateT playGame initialState


