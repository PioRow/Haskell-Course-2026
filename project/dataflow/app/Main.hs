module Main (main) where

import Dflow

main :: IO ()
main = runPipeline "your dataflow graph description here"
