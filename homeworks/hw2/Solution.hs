import Data.Foldable (Foldable(toList),Foldable(length))


data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
---helpers
seqExample :: Sequence Int
seqExample = Append
    (Append (Single 10) (Single 20))
    (Append (Single 30) (Single 40))
seqExample2 :: Sequence Int
seqExample2 = Append
    (Append (Single 50) (Single 60))
    (Append (Single 70) (Empty))

seqExample3 :: Sequence Int
seqExample3 = Append
    (Append (Single 80) (Empty))
    (Append (Single 90) (Single 100))
instance (Show a)=> Show (Sequence a) where
    show Empty=""
    show (Single x) =show x
    show (Append x y) = show x ++","++ show y



--TASK 1



instance Functor Sequence where
    --fmap::(a->b)->Sequence a ->Sequence b
    fmap f Empty=Empty
    fmap f (Single x)= Single (f x)
    fmap f (Append x y )  = Append (fmap f x) (fmap f y)

--TASK 2
instance Foldable Sequence where
    --foldMap::Monoid m => (a->m) ->Sequence a ->m
      foldMap f Empty = mempty
      foldMap f (Single x) = f x
      foldMap f (Append x y) = (foldMap f x)<>(foldMap f y)


seqToList ::Sequence a->[a]
seqToList mySeq = toList mySeq
seqLength :: Sequence a -> Int
seqLength mySeq = length mySeq
--TASK 3
instance Semigroup (Sequence a) where
    --(<>) :: Sequence a -> Sequence a -> Sequence a
    (<>) x y = Append x y
instance Monoid (Sequence a) where
    --mempty :: Sequence a
    mempty= Empty

--TASK 4
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem target mySeq = tailElem' target [mySeq] 
    where
        tailElem' _ [] =False
        tailElem' target (Empty:stack) =tailElem' target stack 
        tailElem' target ((Single x):stack) 
            |target==x = True
            |otherwise = tailElem' target stack 
        tailElem' target ((Append x y):stack)  =tailElem' target (x:y:stack) 
--TASK 5
tailToList :: Sequence a -> [a]
tailToList mySeq= tailToList' [mySeq] []
    where
        tailToList' [] acc=acc
        tailToList' (Empty:stack) acc=tailToList' stack acc
        tailToList' (Single x:stack) acc =tailToList' stack (x:acc)
        tailToList' ((Append x y):stack) acc= tailToList'(y:x:stack) acc

--TASK 6- unchanged
-- data Token = TNum Int | TAdd | TSub | TMul | TDiv
-- tailRPN ::[Token]->Maybe Int
-- tailRPN tokens = tailRPN' tokens [] 
--     where
--         tailRPN' [] [TNum x]=Just x
--         tailRPN' [] (TNum x:stack)=Nothing
--         tailRPN' (TNum x:stack) acc = tailRPN' stack (TNum x:acc)
--         tailRPN' (TAdd:stack) [] = Nothing
--         tailRPN' (TDiv:stack) [] = Nothing
--         tailRPN' (TSub:stack) [] = Nothing
--         tailRPN' (TMul:stack) [] = Nothing
--         tailRPN' (TAdd:stack) [TNum x] = Nothing
--         tailRPN' (TDiv:stack) [TNum x] = Nothing
--         tailRPN' (TSub:stack) [TNum x] = Nothing
--         tailRPN' (TMul:stack) [TNum x] = Nothing
--         tailRPN' (TAdd:stack) (TNum x:TNum y:acc) = tailRPN' stack (TNum (x+y):acc)
--         tailRPN' (TSub:stack) (TNum x:TNum y:acc) = tailRPN' stack (TNum (y-x):acc)
--         tailRPN' (TMul:stack) (TNum x:TNum y:acc) = tailRPN' stack (TNum (x*y):acc)
--         tailRPN' (TDiv:stack) (TNum x:TNum y:acc)
--             | y==0 = Nothing
--             | otherwise = tailRPN' stack (TNum (y `div` x):acc)
--TASK 6- modified
data Ops =TAdd|TSub |TMul| TDiv
data Token = TNum Int | TOp Ops
tailRPN ::[Token]->Maybe Int
tailRPN tokens = tailRPN' tokens [] 
    where
        tailRPN' [] [TNum x]=Just x
        tailRPN' [] (TNum x:stack)=Nothing
        tailRPN' (TNum x:stack) acc = tailRPN' stack (TNum x:acc)
        tailRPN' (TOp _:stack) [] = Nothing
        
        tailRPN' (TOp _:stack) [TNum x] = Nothing
        
        tailRPN' (TOp TAdd:stack) (TNum x:TNum y:acc) = tailRPN' stack (TNum (x+y):acc)
        tailRPN' (TOp TSub:stack) (TNum x:TNum y:acc) = tailRPN' stack (TNum (y-x):acc)
        tailRPN' (TOp TMul:stack) (TNum x:TNum y:acc) = tailRPN' stack (TNum (x*y):acc)
        tailRPN' (TOp TDiv:stack) (TNum x:TNum y:acc)
            | y==0 = Nothing
            | otherwise = tailRPN' stack (TNum (y `div` x):acc)



sampleTokens :: [Token]
sampleTokens = [TNum 10, TNum 2, TOp TAdd, TNum 3,TOp TMul]
sampleTokens2::[Token]
sampleTokens2 = [TNum 5,TOp TAdd]
sampleTokens3::[Token]
sampleTokens3 = [TNum 0, TNum 10,TOp TDiv]
sampleTokens4::[Token]
sampleTokens4 = [TNum 2, TNum 3,TOp TAdd, TNum 2,TOp TMul, TNum 5,TOp TSub]

--TASK 7

exampleList::[Int]
exampleList= [1..17]

myReverse::[a] ->[a]
myReverse list = foldl (\seed x->(x:seed)) [] list 
myTakeWhile:: (a->Bool)->[a]->[a]
myTakeWhile pred list = foldr (\x seed ->if pred x then x:seed else seed ) [] list

decimal::[Int] ->Int
decimal list = foldl (\seed x-> (seed*10 +x) ) 0 list

--TASK 8
encodeElem::Eq a=> a->[(a,Int)]->[(a,Int)]
encodeElem x []= [(x,1)]
encodeElem x ((y,cnt):list)
    |x==y = (x,cnt+1):list
    |otherwise= (x,1):(y,cnt):list


encode ::Eq a=> [a]->[(a,Int)]
encode list = foldr encodeElem [] list

decode ::[(a,Int)]->[a]
decode encds= foldl (\seed (key,cnt)->seed++(replicate cnt key)) [] encds

--example execution
main::IO()

main=do
    print "----sequence example----"
    print $ seqExample

    print "----TASK 1----"
    print $ fmap (+1) seqExample
    print $ fmap show seqExample
    print "----TASK 2----"
    print $ seqToList seqExample
    print $ seqLength seqExample
    print "----TASK 3----"
    print $ seqExample <> seqExample2
    print $ (seqExample <> seqExample2)<>seqExample3
    print $ seqExample <> (seqExample2<>seqExample3)
    print $ mempty <> seqExample
    print $ seqExample <> mempty
    print "----TASK 4----"
    print $ tailElem 20 seqExample
    print $ tailElem 20 seqExample2
    print $ tailElem 80 seqExample3
    print "----TASK 5----"
    print $ tailToList seqExample
    print $ tailToList seqExample2
    print $ tailToList seqExample3
    print "----TASK 6----"
    print $ tailRPN sampleTokens
    print $ tailRPN sampleTokens2
    print $ tailRPN sampleTokens3
    print $ tailRPN sampleTokens4
    print "----TASK 7----"
    print $ myReverse exampleList
    print $ myTakeWhile even exampleList
    print $ myTakeWhile odd exampleList
    print $ decimal [2,1,3,7]
    print "----TASK 8----"
    print $ encode "aaabccca"
    print $ decode (encode "aaabccca")

