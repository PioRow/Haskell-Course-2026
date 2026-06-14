{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module PropertyTests (propTests) where
import qualified Data.Map as M
import Test.Tasty
import Test.Tasty.QuickCheck


import Dflow


instance Arbitrary Node where
  arbitrary = do

    nodeIdInt <- choose (1, 20)::Gen Int
    
    let nodeId' = show nodeIdInt
    
    nodeKind' <- elements ["source", "transform", "sink"]
    return $ Node nodeId' nodeKind' []

instance Arbitrary Program where
  arbitrary = do
    numNodes <- arbitrary
    nodes <- vectorOf numNodes arbitrary
    let nodeIds = map nodeId nodes
        edges = [Edge from' to' | from' <- nodeIds, to' <- nodeIds, from' /= to']
    selectedEdges <- sublistOf edges
    return $ Program nodes selectedEdges


genAcyclicProgram :: Gen Program
genAcyclicProgram = do
  numNodes <- choose (2, 15)::Gen Int
  let nodeIds = [show i | i <- [1..numNodes]]
      nodes'  = [Node i "transform" [] | i <- nodeIds]
      noedeIdsInt = [i | i <- [1..numNodes]]
      potentialEdges = [ Edge (show src) (show tgt) | src <- noedeIdsInt, tgt <- noedeIdsInt,  src < tgt ]
  
  edges' <- sublistOf potentialEdges
  return $ Program nodes' edges'

genCyclicProgram :: Gen Program
genCyclicProgram = do
  numNodes <- choose (3, 10)::Gen Int
  let nodeIds = [show i | i <- [1..numNodes]]
      nodes'  = [Node i "transform" [] | i <- nodeIds]
      spineEdges = zipWith Edge nodeIds (drop 1 nodeIds)
  
  backSrc <- elements (drop 1 nodeIds)
  backTgt <- elements (take (read backSrc - 1) nodeIds)
  let loopEdge = Edge backSrc backTgt
  
  return $ Program nodes' (loopEdge : spineEdges)



data ValidKey = KeyName | KeyWidth | KeyHeight | KeyFormat | KeyFilter | KeyTagged
  deriving (Show, Eq, Enum, Bounded)

showKey :: ValidKey -> String
showKey KeyName   = "name"
showKey KeyWidth  = "width"
showKey KeyHeight = "height"
showKey KeyFormat = "format"
showKey KeyFilter = "filter"
showKey KeyTagged = "tagged"

genValueForKey :: ValidKey -> Gen Value
genValueForKey KeyName   = StrVal  <$> elements ["MainCam", "BackupCam", "Webcam"]
genValueForKey KeyFormat = StrVal  <$> elements ["png", "jpeg", "raw"]
genValueForKey KeyWidth  = NumVal  <$> (fromIntegral <$> (choose (1, 3840) :: Gen Int))
genValueForKey KeyHeight = NumVal  <$> (fromIntegral <$> (choose (1, 2160) :: Gen Int))
genValueForKey KeyTagged = BoolVal <$> chooseAny
genValueForKey KeyFilter = do
  numList <- listOf (NumVal <$> (fromIntegral <$> (choose (1, 100) :: Gen Int)))
  return (ListVal numList)
genPair :: Gen (String, Value)
genPair = do
    key <- elements [KeyName, KeyWidth, KeyHeight, KeyFormat, KeyFilter, KeyTagged]
    value <- genValueForKey key
    return (showKey key, value)
genLinearPipeline :: Gen ([(String,Value)],Program)
genLinearPipeline = do
    numTransforms <- choose (2,30)::Gen Int
    stepPairs<- vectorOf numTransforms genPair
    let stepIds =["node_"++ show i|i<- [1..numTransforms]]
        nodes = zipWith (\name param -> Node name "transform" [param]) stepIds stepPairs
        sourceNode = Node "node_0" "source" []
        sinkNode   = Node ("node_" ++ show (numTransforms + 1)) "sink" []
        allNodes= (sourceNode:nodes)++[sinkNode]
    let allNodesIds= map nodeId allNodes
        allEdges= zipWith Edge allNodesIds (drop 1 allNodesIds)
    return (stepPairs,Program allNodes allEdges)


propTests :: TestTree
propTests = testGroup "Property Tests"
  [ testGroup "Topological Sort Properties"
      [ testProperty "Acyclic graphs  pass topoSort " $
          forAll genAcyclicProgram $ \prog ->
            case topoSort prog of
              Right _ -> True
              Left _  -> False

      , testProperty "Cyclic graph failse on topoSort" $
          forAll genCyclicProgram $ \prog ->
            case topoSort prog of
              Left _  -> True
              Right _ -> False
      ]

  , testGroup "Trimming Properties"
      [ testProperty "Trimming is idempotent: trim(trim(p)) == trim(p)" $
                    \prog -> 
            let firstTrim  = trimProgram prog
                secondTrim = trimProgram firstTrim
            in unpack firstTrim == unpack secondTrim

      , testProperty "Trimming never introduces new nodes or edges" $
          \prog ->
            let Program originalNodes originalEdges = prog
                Program trimmedNodes trimmedEdges  = trimProgram prog
            in (length trimmedNodes <= length originalNodes) && (length trimmedEdges <= length originalEdges)
      ]
    , testGroup "Linear Pipeline composition properties"
      [ testProperty "Linear pipelines are composition of transforms" $ 
           forAll genLinearPipeline $ \(steps,prog)->
                case execute prog of
                    Left err -> error $ "Pipeline execution failed: " ++ err
                    Right photoList -> 
                     let finalActualMap = head photoList
                         initialState = PhotoObj (M.fromList [("id",StrVal "rnd-id-gen")])
                         expectedMap = foldl (\(PhotoObj m) (k, v) -> PhotoObj (M.insert k v m)) 
                                                         initialState 
                                                         steps
                     in 
                        finalActualMap == expectedMap
      ]
  ]


unpack :: Program -> ([Node], [Edge])
unpack (Program ns es) = (ns, es)