import qualified Data.List as L
import System.CPUTime (getCPUTime)
import System.Mem (performGC)
import GHC.Stats (getRTSStatsEnabled, getRTSStats, allocated_bytes, max_mem_in_use_bytes)
import Data.Ord (comparing)
import Data.List (sortBy)

-- ...existing code...
-- (your function definitions for myTailQuickSort, tailQuickSort, genSmallList, genVeryLargeList, etc.)

-- Enhanced timing with memory profiling
timeItMem :: String -> IO a -> IO a
timeItMem label action = do
    performGC
    statsEnabled <- getRTSStatsEnabled
    startCpu <- getCPUTime

    if not statsEnabled
      then do
        result <- action
        endCpu <- getCPUTime
        let cpuTime = (fromIntegral (endCpu - startCpu) / 1e12) :: Double
        putStrLn $ label ++ " - CPU: " ++ show cpuTime ++ "s | MEM: enable with +RTS -T"
        pure result
      else do
        s0 <- getRTSStats
        result <- action
        performGC
        s1 <- getRTSStats
        endCpu <- getCPUTime

        let cpuTime = (fromIntegral (endCpu - startCpu) / 1e12) :: Double
            allocMB = (fromIntegral (allocated_bytes s1 - allocated_bytes s0) / (1024 * 1024)) :: Double
            maxMemMB = (fromIntegral (max_mem_in_use_bytes s1) / (1024 * 1024)) :: Double

        putStrLn $ label
            ++ " - CPU: " ++ show cpuTime ++ "s"
            ++ " | Alloc: " ++ show allocMB ++ " MB"
            ++ " | MaxMemInUse: " ++ show maxMemMB ++ " MB"
        pure result

-- ...existing code...
data StackElem a = Unsorted [a] | Sorted a 

tailQuickSort :: Ord a => [a] -> [a]
tailQuickSort list = go [Unsorted list] [] 
  where
   go [] acc = acc 
   go (Sorted elem : stack) acc = go stack (elem : acc)
   go (Unsorted [] : stack) acc = go stack acc 
   go (Unsorted (x:xs) : stack) acc = 
    let smaller = filter (< x) xs
        bigger  = filter (>= x) xs
    in go (Unsorted bigger : Sorted x : Unsorted smaller : stack) acc

myTailQuickSort :: Ord a => [a] -> [a]
myTailQuickSort list =go list []
  where
    go [] sorted=sorted
    --go [x] sorted=sorted++[x]
    go (x:xs) sorted=
      let
        leq= go (filter(<=x) xs) sorted
        ge=filter(>x) xs
        in 
          go ge (leq++[x])
-- Enhanced timing with memory info

-- Test data generators
genSmallList :: [Int]
genSmallList = [3, 1, 4, 1, 5, 9, 2, 6]

genMediumList :: [Int]
genMediumList = [10, 7, 3, 15, 2, 9, 8, 1, 4, 6, 5, 11, 12, 13, 14]

genLargeList :: [Int]
genLargeList = [100, 50..1] ++ [101..150]

genVeryLargeList :: [Int]
genVeryLargeList = [10000, 9999..1]

genHugeList :: [Int]
genHugeList = [50000, 49999..1]

genRandomList :: [Int]
genRandomList = take 1000 (cycle [42, 17, 89, 3, 71])

-- Comprehensive correctness tests


-- Profile performance
profileFunctions :: IO ()
profileFunctions = do
    putStrLn "\n=== Performance Profiling ==="
    putStrLn "Small (8 elements):"
    timeItMem "  myTailQuickSort" (print $ length $ myTailQuickSort genSmallList)
    timeItMem "  tailQuickSort" (print $ length $ tailQuickSort genSmallList)
    
    putStrLn "\nMedium (15 elements):"
    timeItMem "  myTailQuickSort" (print $ length $ myTailQuickSort genMediumList)
    timeItMem "  tailQuickSort" (print $ length $ tailQuickSort genMediumList)
    
    putStrLn "\nLarge (150 elements):"
    timeItMem "  myTailQuickSort" (print $ length $ myTailQuickSort genLargeList)
    timeItMem "  tailQuickSort" (print $ length $ tailQuickSort genLargeList)
    
    putStrLn "\nVery Large (10000 elements):"
    timeItMem "  myTailQuickSort" (print $ length $ myTailQuickSort genVeryLargeList)
    timeItMem "  tailQuickSort" (print $ length $ tailQuickSort genVeryLargeList)
    
    putStrLn "\nRandom (1000 elements):"
    timeItMem "  myTailQuickSort" (print $ length $ myTailQuickSort genRandomList)
    timeItMem "  tailQuickSort" (print $ length $ tailQuickSort genRandomList)


-- run with +RTS -T -s
main :: IO ()
main = do
    profileFunctions
