module Dflow 
  ( 
   module Dflow.Types
  , module Dflow.Parser
  ,module Dflow.Validator
  ,module Dflow.Executor
  ,mainJob
  ) where

import Dflow.Types
import Dflow.Parser
import Dflow.Validator
import Dflow.Executor
import Text.Megaparsec 

mainJob :: String -> IO ()
mainJob filePath = do
    input <- readFileAsText filePath
    
    -- We wrap the Either pipeline in a clean handler
    case runPipeline input of
      Left err  -> putStrLn $ "Error: " ++ err
      Right ret -> do
        putStrLn "Program executed successfully."
        putStrLn $ "Result: " ++ ret
  where
    -- ⚡ Flat Monadic Flow: Any Left instantly exits the computation!
    runPipeline txt = do
      prog      <- case parse programP filePath txt of
                     Left err -> Left (errorBundlePretty err)
                     Right p  -> Right p
      validProg <- validateProgram prog
      execute validProg
    
    