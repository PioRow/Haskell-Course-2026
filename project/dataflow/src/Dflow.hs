module Dflow 
  ( runPipeline
  , module Dflow.Types
  ) where

import Dflow.Types
import Dflow.Parser
import Dflow.Evaluator

-- | Ties the whole pipeline together
runPipeline :: String -> IO ()
runPipeline input = case parseDataflow input of
  Left err   -> putStrLn $ "Parse Error: " ++ err
  Right ast  -> evaluate (Program ast []) -- For now, we ignore edges