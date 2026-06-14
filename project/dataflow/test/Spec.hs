module Main (main) where

import Test.Tasty
import qualified UnitTests
import qualified EndToEndTests
import qualified PropertyTests
main :: IO ()
main = defaultMain masterTestSuite

masterTestSuite :: TestTree
masterTestSuite = testGroup "Dflow Compiler Test Suite"
  [ UnitTests.unitTests,
    EndToEndTests.e2eTests,
    PropertyTests.propTests
  ]