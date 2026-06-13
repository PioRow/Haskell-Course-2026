{-# LANGUAGE OverloadedStrings #-}
module EndToEndTests (e2eTests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M

-- Import your core engine types
import Dflow


e2eTests :: TestTree
e2eTests = testGroup "End-to-End Pipeline Tests"
  [ testCase "Pipeline fails on parsing" $ do
      let script = "source cam { width: 1920\n\
                   \sink final from cam {}" 
      
      case runPipeline "" script of
        Right _  -> assertFailure "Pipeline should have failed due to syntax error."
        Left err -> assertBool "Error message should mention syntax or parsing" 
                    (not (null err))

  , testCase "Pipeline fails on cyclic validation" $ do
      let script = "source cam { width: 1920 }\n\
                   \transform loopNode from resize { width: 640 }\n\
                   \transform resize from loopNode { width: 320 }\n\
                   \sink final from resize {}"
      
      case runPipeline "" script of
        Right _  -> assertFailure "Pipeline should have aborted due to a cyclic loop."
        Left _   -> return () -- Success: Correctly rejected the cycle

  , testCase "Pipeline succeeds with valid output" $ do
      let script = "source camera { name: \"MainCam\", width: 1920 }\n\
                   \transform filterNode from camera { height: 1080, format: \"png\", tagged: true }\n\
                   \sink viewer from filterNode {}"
      
      case runPipeline "" script of
        Left err -> assertFailure $ "Pipeline failed unexpectedly with error: " ++ err
        Right photoList -> do
          -- 1. Ensure we actually received generated output items in our collection list
          assertBool "sink produces output" (not (null photoList))
          
          -- Extract the first element's internal map wrapper for field testing
          let (PhotoObj firstMap) = head photoList
          
          -- Field 1: String type check ("name")
          assertEqual "field matches source parameter" 
                      (Just (StrVal "MainCam")) 
                      (M.lookup "name" firstMap)
          
          -- Field 2: Numeric type check ("height")
          assertEqual "field matches transform parameter" 
                      (Just (NumVal 1080.0)) 
                      (M.lookup "height" firstMap)
          
          -- Field 3: Boolean type check ("tagged")
          assertEqual "field matches transform parameter" 
                      (Just (BoolVal True)) 
                      (M.lookup "tagged" firstMap)
  ]