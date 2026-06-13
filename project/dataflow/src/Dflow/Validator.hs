module Dflow.Validator (
    validateProgram,
    topoSort,
    trimProgram
) where
import Dflow.Types
import qualified Data.Map as M
import qualified Data.Set as S

type AdjList = M.Map String [String]




buildForwardAdj :: [Edge] -> AdjList
buildForwardAdj edges = M.fromListWith (++) [(from e, [to e]) | e <- edges]

buildBackwardAdj :: [Edge] -> AdjList
buildBackwardAdj edges = M.fromListWith (++) [(to e, [from e]) | e <- edges]


topoSort:: Program -> Either String [Node]
topoSort prog =
    let edges = getEdges prog
        nodes = getNodes prog
        forwardAdj = buildForwardAdj edges
        nodeMap = M.fromList [(nodeId n, n) | n <- nodes]
        inDegree = M.fromListWith (+) [(to e, 1) | e <- edges]
        zeroInDegree = [n | n <- nodes, M.notMember (nodeId n) inDegree]
    in go zeroInDegree [] inDegree forwardAdj nodeMap
    where
        go :: [Node] -> [Node] -> M.Map String Int -> M.Map String [String] -> M.Map String Node -> Either String [Node]
        go [] sorted _ _ _ 
            | length sorted == length (getNodes prog) = Right (reverse sorted)
            | otherwise = Left "Graph has a cycle"
        go (n:ns) sorted inDegree forwardAdj nodeMap =
            let neigs = M.findWithDefault [] (nodeId n) forwardAdj
                
                
                updateNeighbors [] currentInDegree currentQueue = 
                    (currentInDegree, currentQueue)
                updateNeighbors (childId:cs) currentInDegree currentQueue =
                    let currentDeg = M.findWithDefault 0 childId currentInDegree
                        newDeg = currentDeg - 1
                        nextInDegree = M.insert childId newDeg currentInDegree
                        nextQueue =
                            if newDeg == 0
                            then case M.lookup childId nodeMap of
                                    Just childNode -> childNode : currentQueue
                                    Nothing        -> currentQueue
                            else currentQueue
                    in updateNeighbors cs nextInDegree nextQueue

                (updatedInDegree, newlyFreedNodes) = updateNeighbors neigs inDegree ns
                
            in go newlyFreedNodes (n : sorted) updatedInDegree forwardAdj nodeMap
trimProgram :: Program -> Program
trimProgram prog =
    let edges = getEdges prog
        nodes = getNodes prog
        sources = [nodeId n | n <- nodes, nodeKind n == "source"]
        sinks   = [nodeId n | n <- nodes, nodeKind n == "sink"]
        forwardAdj  = buildForwardAdj edges   
        backwardAdj = buildBackwardAdj edges  
        reachableFromSources = findReachable sources forwardAdj
        reachableToSinks     = findReachable sinks backwardAdj
        validNodeIds = S.intersection reachableFromSources reachableToSinks
        trimmedNodes = [n | n <- nodes, nodeId n `S.member` validNodeIds]
        trimmedEdges = [e | e <- edges, from e `S.member` validNodeIds && to e `S.member` validNodeIds]
    in Program trimmedNodes trimmedEdges

findReachable :: [String] -> M.Map String [String] -> S.Set String
findReachable starts adj = go starts S.empty
  where
    go [] visited = visited
    go (curr:queue) visited
      | curr `S.member` visited = go queue visited
      | otherwise =
          let neighbors = M.findWithDefault [] curr adj
          in go (neighbors ++ queue) (S.insert curr visited)

validateProgram :: Program -> Either String Program
validateProgram prog =do 
    _ <- topoSort prog
    let trimmedProg = trimProgram prog
    sortedNodes <- topoSort trimmedProg
    return $ Program sortedNodes (getEdges trimmedProg)

