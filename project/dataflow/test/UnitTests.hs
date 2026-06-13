{-# LANGUAGE OverloadedStrings #-}

module UnitTests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (parse)
import qualified Data.Map as M
import Dflow
import qualified Data.Text as T
unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ parserTests,
    cycleTests,
    trimmingTests
  ]
parserTests :: TestTree
parserTests = testGroup "Parser Correctness"
  [ (testCase "Parses a simple source block successfully" $ do
      let input = "source cam { width: 1920 }"
      case parse programP "" input of
        Left err -> assertFailure $ "Parser failed on valid syntax: " ++ show err
        Right (Program [Node kindid kind params] _) -> do
          assertEqual "Extracts correct ID" "cam" kindid
          assertEqual "Extracts correct Kind" "source" kind
          assertEqual "Extracts parameter" (Just (NumVal 1920)) (lookup "width" params)
        Right _ -> assertFailure "Parsed incorrect node layout structure.")
    ,
    (testCase "Parses a chain successfully" $ do
      let input = T.unlines
            [ "source cam { width: 1920 }"
            , "transform resize from cam { width: 1280 }"
            , "sink display from resize {}"
            ]
      case parse programP "" input of
        Left err -> assertFailure $ "Parser failed on valid syntax: " ++ show err
        Right (Program nodes edges) -> do
          assertEqual "Extracts correct number of nodes" 3 (length nodes)
          assertEqual "Extracts correct number of edges" 2 (length edges)
          let nodeMap = M.fromList [(nodeId n, n) | n <- nodes]
          case M.lookup "cam" nodeMap of
            Nothing -> assertFailure "Missing 'cam' node"
            Just (Node kindid kind params) -> do
              assertEqual "Extracts correct ID for cam" "cam" kindid
              assertEqual "Extracts correct Kind for cam" "source" kind
              assertEqual "Extracts parameter for cam" (Just (NumVal 1920)) (lookup "width" params)
          case M.lookup "resize" nodeMap of
            Nothing -> assertFailure "Missing 'resize' node"
            Just (Node kindid kind params) -> do
              assertEqual "Extracts correct ID for resize" "resize" kindid
              assertEqual "Extracts correct Kind for resize" "transform" kind
              assertEqual "Extracts parameter for resize" (Just (NumVal 1280)) (lookup "width" params)
          case M.lookup "display" nodeMap of
            Nothing -> assertFailure "Missing 'display' node"
            Just (Node kindid kind params) -> do
              assertEqual "Extracts correct ID for display" "display" kindid
              assertEqual "Extracts correct Kind for display" "sink" kind
              assertEqual "Extracts parameter for display" Nothing (lookup "width" params)),
    (testCase "Fails on missing 'from' in transform" $ do
      let input = "transform resize { width: 1280 }"
      case parse programP "" input of
        Left _ -> return ()
        Right _ -> assertFailure "Expected parser error for missing 'from' clause"),
    (testCase "Fails on missing body" $ do
      let input = "source cam{ width: 1920 } transform resize from cam"
      case parse programP "" input of
        Left _ -> return ()
        Right _ -> assertFailure "Expected parser error for missing body"),
    (testCase " accepts comments and blocks" $ do
      let input = T.unlines
            [ "// This is a comment"
            , "source cam { width: 1920 }"
            , "/* Multi-line comment\n spanning multiple lines */"
            , "// Another comment"
            , "transform resize from cam { width: 1280 }"
            , "sink display from resize {}"
            ]
      case parse programP "" input of
        Left err -> assertFailure $ "Parser failed on valid syntax with comments: " ++ show err
        Right (Program nodes edges) -> do
          assertEqual "Extracts correct number of nodes" 3 (length nodes)
          assertEqual "Extracts correct number of edges" 2 (length edges)),
    (testCase "Parses multiple parameters" $ do
      let input = "source cam { width: 1920, height: 1080, format: \"jpg\" }"
      case parse programP "" input of
        Left err -> assertFailure $ "Parser failed on valid syntax with multiple parameters: " ++ show err
        Right (Program [Node kindid kind params] _) -> do
          assertEqual "Extracts correct ID" "cam" kindid
          assertEqual "Extracts correct Kind" "source" kind
          assertEqual "Extracts width parameter" (Just (NumVal 1920)) (lookup "width" params)
          assertEqual "Extracts height parameter" (Just (NumVal 1080)) (lookup "height" params)
          assertEqual "Extracts format parameter" (Just (StrVal "jpg")) (lookup "format" params)
        Right _ -> assertFailure "Parsed incorrect node layout structure."),
    (testCase "Parses nonlinear piplines" $ do
      let input = T.unlines
            [ "source cam1 { width: 1920 }"
            , "transform resize1 from cam1 { width: 1280 }"
            , "transform resize2 from cam1 { width: 640 }"
            , "sink display1 from resize1 {}"
            , "sink display2 from resize2 {}"
            ]
      case parse programP "" input of
        Left err -> assertFailure $ "Parser failed on valid syntax with nonlinear pipelines: " ++ show err
        Right (Program nodes edges) -> do
          assertEqual "Extracts correct number of nodes" 5 (length nodes)
          assertEqual "Extracts correct number of edges" 4 (length edges))
  ]

     
    
cycleTests :: TestTree
cycleTests = testGroup "Cycle Detection"
  [
    testCase "reject cycle  cycle" $ do
      let 
        nodes =[(Node "cam"         "source"    [("width", NumVal 1920.0)])
          , (Node "loop"        "transform" [("width", NumVal 640.0)])  
          , (Node "resize"      "transform" [("width", NumVal 320.0)])
          , (Node "final"       "sink"      [])
          ]
      
        edges = 
          [ (Edge "cam" "resize")
          , (Edge "resize" "loop")
          , (Edge "loop"        "resize")
          , (Edge "resize"      "final") 
          ]
        cycled = Program nodes edges
      
      case topoSort cycled of
        Left _ -> return ()
        Right _ -> assertFailure "Expected cycle detection error for simple cycle"
    ,
    testCase "pass acyclic graph" $ do
      let nodes = [ (Node "cam"         "source"    [("width", NumVal 1920.0)]), (Node "loop"        "transform" [("width", NumVal 640.0)]), (Node "resize"      "transform" [("width", NumVal 320.0)]), (Node "final"       "sink"      [])]       
          edges = [ (Edge "cam" "resize"), (Edge "resize" "loop"), (Edge "loop" "final")]
          acyclic = Program nodes edges
      case topoSort acyclic of
        Left _ -> assertFailure "Expected acyclic graph to pass topological sort"
        Right _ -> return ()
    
  ]
trimmingTests :: TestTree
trimmingTests = testGroup "Graph Trimming "
  [ testCase "Removes nodes without sink and source" $ do
      let nodes = 
            [ Node "cam"   "source"    [("width", NumVal 1920.0)]
            , Node "final" "sink"      []
            , Node "ghost" "transform" [("width", NumVal 500.0)] -- ⚡ Dangling
            ]
          edges = 
            [ Edge "cam" "final" 
            ]
          prog = Program nodes edges
          
          Program trimmedNodes trimmedEdges = trimProgram prog
          trimmedNodeIds = map nodeId trimmedNodes

      assertEqual "Keeps the active source node" True  ("cam" `elem` trimmedNodeIds)
      assertEqual "Keeps the active sink node"   True  ("final" `elem` trimmedNodeIds)
      assertEqual "Prunes the dangling node"     False ("ghost" `elem` trimmedNodeIds)
      assertEqual "Maintains active edges"       1     (length trimmedEdges)

  , testCase "keeps valid paths from source to sink" $ do
      let nodes = 
            [ Node "cam"      "source"    [("width", NumVal 2048.0)]
            , Node "scale"    "transform" [("width", NumVal 1024.0)]
            , Node "sharpen"  "transform" []
            , Node "v_viewer" "sink"      []
            , Node "f_saver"  "sink"      []
            ]
          -- Branching pipeline: cam -> scale -> sharpen -> both sinks
          edges = 
            [ Edge "cam"     "scale"
            , Edge "scale"   "sharpen"
            , Edge "sharpen" "v_viewer"
            , Edge "sharpen" "f_saver"
            ]
          prog = Program nodes edges
          
          Program trimmedNodes trimmedEdges = trimProgram prog
          trimmedNodeIds = map nodeId trimmedNodes

      assertEqual "Keeps source node"     True ("cam" `elem` trimmedNodeIds)
      assertEqual "Keeps transform 1"    True ("scale" `elem` trimmedNodeIds)
      assertEqual "Keeps transform 2"    True ("sharpen" `elem` trimmedNodeIds)
      assertEqual "Keeps display sink"   True ("v_viewer" `elem` trimmedNodeIds)
      assertEqual "Keeps file save sink" True ("f_saver" `elem` trimmedNodeIds)
      assertEqual "No edges are removed" 4    (length trimmedEdges)
  ]
