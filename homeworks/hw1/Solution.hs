import Data.List (permutations)
import Control.Monad (when)
--Solution for Homework 1
--Task 3
sieve :: [Int] -> [Int]
sieve []=[]
sieve (x:xs)= x:sieve [y| y<-xs, y `mod` x /=0]
primesTo :: Int ->[Int]
primesTo n= sieve [2..n]
isPrime :: Int -> Bool
isPrime n = n `elem` (primesTo n)
--Task 1
goldbachPairs ::Int -> [(Int,Int)]
goldbachPairs n=[(x,n-x)|x<-[2..(n `div`2)],isPrime x,isPrime (n-x)]
--Task 2
coprimePairs ::[Int]-> [(Int,Int)]
coprimePairs list= [(x,y)|x<-list,y<-list,x<y,gcd x y ==1]


--Task 4
matMul ::[[Int]]->[[Int]] ->[[Int]]
matMul a b =
    let
    p = length (head a)
    n= length (head b)
    m= length a
    in
        [[sum [ a !! i !! k * b !! k !! j | k <- [0 .. p-1] ] |j<-[0..n-1]]|i<-[0..m-1]]



--Task 5
outOne::[a]->[(a,[a])]
outOne []=[]
outOne (x:xs)= (x,xs):[(y,x:ys)|(y,ys)<-outOne xs]

permutations'::Int->[a]-> [[a]]
permutations' 0 _ = [[]]
permutations' _ [] = []
permutations' n list= [x:ps| (x,xs)<-outOne list, ps<-permutations' (n-1) xs]

--Task 6
merge :: Ord a => [a] -> [a] -> [a]
merge l1 [] = l1
merge [] l2 = l2
merge (x:xs) (y:ys)
    |x==y = x:merge xs ys
    | x<y = x :merge xs (y:ys)
    |otherwise = y :merge (x:xs) ys

humming :: [Int]
humming = 1:merge [2,4..] (merge [3,6..] [5,10..])
--Task 7
power :: Int -> Int -> Int
power b e = power' b e 1
    where
        power' _ 0 !acc=acc
        power' b e !acc = power' b (e-1) (acc*b)


--Task 8

listMaxSeq::[Int]->Int
listMaxSeq []= error "WTF"
listMaxSeq (x:xs)= listMaxSeq' xs x
    where
        listMaxSeq' [] max_n=max_n
        listMaxSeq' (x:xs) max_n= 
            let cur_max= max max_n x
            in seq cur_max (listMaxSeq' xs cur_max)

listMaxBang::[Int]->Int
listMaxBang []= error "WTF"
listMaxBang (x:xs)=listMaxBang' xs x
    where 
        listMaxBang' [] !max_n=max_n
        listMaxBang' (x:xs) !max_n = listMaxBang' xs (max x max_n)


--Task 9
primes::[Int]
primes = sieve [2..]

isPrimeStream::Int -> Bool
isPrimeStream n = isPrimeStream' n primes
    where 
        isPrimeStream' n (p:ps)
            | p == n = True
            | p > n = False
            | otherwise = isPrimeStream' n ps


meanLazy::[Double]->Double
meanLazy list= meanLazy' list 0 0 
    where
        meanLazy' [] sum count =sum/count
        meanLazy' (x:xs) sum count = meanLazy' xs (sum+x) (count+1)


meanStrict::[Double]->Double
meanStrict list= meanStrict' list 0 0 
    where
        meanStrict' [] !sum !count =sum/count
        meanStrict' (x:xs) !sum !count = meanStrict' xs (sum+x) (count+1)


statistics::[Double]->(Double,Double)
statistics list= meanStrict' list 0 0 0
    where
        meanStrict' [] !sum !sos !count =
            let !mean=sum/count
            in (mean,sos/count-mean*mean)
        meanStrict' (x:xs) !sum !sos !count =
             meanStrict' xs (sum+x) (sos+ x*x) (count+1)

--Task 10 
-- example usage
main:: IO ()

main = do
    print ( isPrime 37)
    print (isPrime 51)
    print (coprimePairs [1..15])
    print (goldbachPairs 14)
    print (matMul [[1,2,3],[1,2,3]] [[2,1],[3,7],[6,7]] )
    print (permutations' 2 [1,2,3])
    print (take 4 (merge [1,3..] [2,4..]))
    print (take 11 humming)
    print (power 3 4)
    print (listMaxSeq [2,3,1,6,7,67])
    print (listMaxBang [2,3,1,6,7,67])
    print (isPrimeStream 38)
    print(meanLazy [1,2,3,4,5,6,7,6,9])
    print(meanStrict [1,2,3,4,5,6,7,6,9])
    print(statistics [1,2,3,4,5,6,7,6,9])