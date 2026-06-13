module Dflow 
  ( 
   module Dflow.Types
  , module Dflow.Parser
  ,module Dflow.Validator
  ,module Dflow.Executor
  ,mainJob
  ,runPipeline
  ) where

import Dflow.Types
import Dflow.Parser
import Dflow.Validator
import Dflow.Executor
import Text.Megaparsec 
import Data.Text as T
mainJob :: String -> IO ()

runPipeline :: String -> T.Text -> Either String [PhotoObj]
runPipeline filePath txt = do
      prog <- case parse programP filePath txt of
              Left err -> Left (errorBundlePretty err)
              Right p  -> Right p
      validProg <- validateProgram prog
      execute validProg

mainJob filePath = do
    input <- readFileAsText filePath
    
    -- We wrap the Either pipeline in a clean handler
    case runPipeline filePath input of
      Left err  -> putStrLn $ "Error: " ++ err
      Right ret -> do
        putStrLn "Program executed successfully."
        putStrLn $ "Result: " ++ show ret
    -- ⚡ Flat Monadic Flow: Any Left instantly exits the computation!
    
    
    