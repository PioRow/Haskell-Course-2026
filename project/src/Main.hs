module Main (main) where


exFun::[Int] -> Int
exFun []= -1
exFun (x:xs)=let rest=exFun xs in max x rest

main :: IO ()
main = do
    print (exFun [1,2,3,4,5])
