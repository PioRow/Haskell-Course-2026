{- HLINT ignore "Avoid lambda using `infix`" -}
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Internal.Encoding.Utf32 (validate)
import Control.Monad (guard)
import Data.List (delete)
import Distribution.Compat.CharParsing (CharParsing(string))
import Distribution.Simple.Utils (xargs)

type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)
-- TASK1
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
    dirMap<- M.lookup pos maze
    pospos<-  M.lookup dir dirMap
    return pospos

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze pos [] = Just pos
followPath maze pos (dir:dirs)=do
    npos<-move maze pos dir
    followPath maze npos dirs
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze pos [] = Just []
safePath maze pos (dir:dirs)=do
    npos<-move maze pos dir
    path<-safePath maze npos dirs
    return (pos:path)
exampleMaze :: Maze
exampleMaze = M.fromList
    [ ((0,0), M.fromList [(N, (0,1)), (E, (1,0))])
    , ((0,1), M.fromList [(S, (0,0)), (E, (1,1))])
    , ((1,0), M.fromList [(N, (1,1)), (W, (0,0))])
    , ((1,1), M.fromList [(S, (1,0)), (W, (0,1))])
    ]
testMove1 = move exampleMaze (0,0) N
testMove2 = move exampleMaze (0,0) W
testPath1 = followPath exampleMaze (0,0) [E, N]
testPath2 = followPath exampleMaze (0,0) [N, N]
testSafe1 = safePath exampleMaze (0,0) [N, E, S]
testSafe2 = safePath exampleMaze (1,1) [W, W, S]

--TASK2
type Key = Map Char Char
decrypt :: Key -> String -> Maybe String
decrypt key = traverse (\c->M.lookup c key)
decryptWords :: Key -> [String] -> Maybe [String]

decryptWords key []=Just []
decryptWords key (str:strs) = do
    decoded<-decrypt key str
    rest<-decryptWords key strs
    return (decoded:rest)


testKey :: Key
testKey = M.fromList [('d', 'a'), ('e', 'b'), ('f', 'c')]
testk1 = decrypt testKey "def"
testk2 = decrypt testKey "dez"
testk3 = decrypt testKey ""
testdW1= decryptWords testKey ["def", "fed"]
testdW2= decryptWords testKey ["def", "xas"]

--TASK3
type Guest = String
type Conflict =  (Guest, Guest)
validPair :: [Conflict] -> Guest -> Guest -> Bool
validPair conflicts g1 g2 = not $ (g1, g2) `elem` conflicts
     || (g2, g1) `elem` conflicts

validPermutation :: [Conflict] -> [Guest] -> Bool
validPermutation _ [] = True
validPermutation _ [x] = True
validPermutation conflicts (x:y:xs)=
    validPair conflicts x y && validPermutation conflicts xs

generatePerm ::[Guest]->[[Guest]]
generatePerm [] = [[]]
generatePerm guests=do
    g<-guests
    other <-generatePerm (delete g guests)
    return (g:other)

seating ::[Guest]->[Conflict]->[[Guest]]
seating guests conflicts =do
    (x:xs)<-generatePerm guests
    guard (validPermutation conflicts (x:xs))
    guard (validPermutation conflicts (xs++[x]))
    return (x:xs)

guests = ["A", "B", "C","D"]
conflicts = [("A", "B")]

--TASK4
data Result a= Failure String |Success a [String] deriving (Show)

instance Functor Result where
    fmap :: (a -> b) -> Result a -> Result b
    fmap _ (Failure str)=Failure str
    fmap f (Success x strs)=Success (f x) strs

instance Applicative Result where
    pure x =Success x []
    (<*>) (Failure str) _ =Failure str 
    
    (<*>) _ (Failure str) =Failure str
    (<*>) (Success f flog) (Success x log)= Success (f x) (log++flog) 

instance Monad Result where
    return  =pure
    (Failure str)>>=  _= Failure str
    (Success x log)>>= f =
        case  f x of
        Success y ylog -> Success y (log++ylog)
        Failure ylog ->Failure ylog

warn ::String ->Result ()
warn str= Success () [str]
failure :: String -> Result a
failure str = Failure str


validateAge::Int ->Result Int
validateAge age
    |age <0= Failure "no negative age"
    |age>150 =Success age ["lil Old"]
    |otherwise= Success age []


validateAges :: [Int] -> Result [Int]
validateAges []=Success [] []
validateAges (x:xs) =do
    first<-validateAge x
    rest<-validateAges xs
    return (first:rest)


--TASK 5

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr 
 deriving(Show)  


newtype Writer m a = Writer {runWriter :: (a,m)} deriving (Show)

instance (Monoid m)=>Functor (Writer m) where
    fmap :: Monoid m => (a -> b) -> Writer m a -> Writer m b
    fmap f (Writer (x,m)) = Writer(f x,m)
    
instance (Monoid m)=>Applicative (Writer m) where
    pure x = Writer(x,mempty)
    Writer(f,fm) <*> Writer(x,xm)=Writer(f x,xm<>fm)

instance (Monoid m)=>Monad(Writer m) where
    (Writer(x,xm)) >>= f = let 
        Writer(y,ym) =f x 
        in Writer(y,xm<>ym)

write :: m -> Writer m ()
write message = Writer ((), message)


simplyfy ::Expr ->Writer [String] Expr
simplyfy (Lit i)=return (Lit i)
simplyfy (Neg (Neg e))=do
    write ["Double negation: --e -> e"]
    simplyfy e
simplyfy (Neg (Lit i))=do
    return (Lit (-i))
simplyfy (Neg exp)=do
    expS<-simplyfy exp
    simplyfy (Neg expS) -- switch to recursive call to simplyfy to single node
simplyfy (Add (Lit 0) e) =do
    newE<-simplyfy e
    write ["Add identity: 0 + "++show newE++" -> "++show newE]
    return newE
simplyfy (Add e (Lit 0) ) =do
    newE<-simplyfy e
    write ["Add identity:  "++show newE++"+ 0 -> "++show newE]
    return newE
simplyfy (Add (Lit i) (Lit j))= do
    write ["constant folding: "++show i ++" + "++show j++" -> "++show (i+j)]
    return (Lit (i+j))
simplyfy (Add ex1 ex2)=do 
    ex1S<-simplyfy ex1
    ex2S<-simplyfy ex2 
    simplyfy (Add ex1S ex2S)-- switch to recursive call to simplyfy to single node
simplyfy (Mul (Lit 1) e) =do
    newE<-simplyfy e
    write ["multiplicative identity: 1 * "++show newE++" -> "++show newE]
    return newE
simplyfy (Mul e (Lit 1) ) =do
    newE<-simplyfy e
    write ["multiplicative identity:  "++show newE++" * 1 -> "++show newE]
    return newE
simplyfy (Mul (Lit 0) e) =do
    write ["zero absorption"]
    return (Lit 0)
simplyfy (Mul e (Lit 0) ) =do
    write ["zero absorption"]
    return (Lit 0)
simplyfy (Mul (Lit i) (Lit j))= do
    write ["constant folding: "++show i ++" * "++show j++" -> "++show (i*j)]
    return (Lit (i*j))
simplyfy (Mul ex1 ex2)=do 
    ex1S<-simplyfy ex1
    ex2S<-simplyfy ex2 
    simplyfy (Mul ex1S ex2S)-- switch to recursive call to simplyfy to single node



expExample1 = Add (Add (Lit 1) (Add (Lit 2) (Lit 3))) (Lit 3)

--TASK 6
newtype ZipList a = ZipList {getZipList::[a]} deriving( Show)

instance Functor ZipList where
  fmap f (ZipList zlist) = ZipList (fmap f zlist)

instance Applicative ZipList where
    --pure  :: a -> f a
    --(<*>) :: f (a -> b) -> f a -> f b
    pure x= ZipList (repeat x)
    (<*>) (ZipList []) _ = ZipList []
    (<*>) _(ZipList []) = ZipList []
    (<*>) (ZipList (f:fs)) (ZipList (x:xs)) = let
        ZipList res=(<*>) (ZipList (fs)) (ZipList (xs))
        in 
            ZipList ((f x):res)

{- 
   as the hint says, the problem lies with >>= function, as we already defined pure,
   lets say a=ZipList [1,2,3]
   >>=)  :: m a -> (a -> m b) -> m b
   g x = ZipList (replicate x x)
   there is no natural way to compose resulting ZipLists into one
-}
--instance Monad ZipList where


main ::IO()
main=do
    print $ "TASK 1"
    print $ testMove1
    print $ testMove2
    print $ testPath2
    print $ testPath1
    print $ testSafe1
    print $ testSafe2

    print $ "TASK 2"
    print $ testk1
    print $ testk2
    print $ testk3

    print $ testdW1
    print $ testdW2
    print $ "TASK 3"
    print $ seating guests conflicts

    print $ "TASK 4"
    print $ validateAges[ 1 ,160,2137]
    print $ validateAges[ 1 ,16,21]
    print $ validateAges[ 1 ,160,-2137]
    print $ "TASK 5"
    print $ simplyfy expExample1
    print $ simplyfy (Add (Lit 5) (Lit 10))
    
    
    print $ simplyfy (Neg (Neg (Add (Lit 0) (Lit 5))))
    
    
    print $ simplyfy (Add (Mul (Lit 1) (Lit 5)) (Mul (Lit 10) (Lit 0)))
    
    
    print $ simplyfy (Mul (Add (Lit 2) (Lit 3)) (Mul (Lit 1) (Lit 4)))
    print $ "TASK 6"
    print $ fmap (*2) (ZipList [1..3]) 
    print $ fmap (*2) (ZipList []) 
    print $ pure id <*> ZipList [1,2,3]                          
    print $ pure (+) <*> ZipList [1,2,3] <*> ZipList [10,20,30]