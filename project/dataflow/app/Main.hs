module Main (main) where

import Dflow (mainJob)

import System.Environment (getArgs)
import System.IO (hIsEOF, stdin)




main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      mainJob filePath
    _ -> do
      putStrLn "No file provided. Starting REPL mode."
      repl

repl :: IO ()
repl = do
  putStrLn "dataflow> (Ctrl-D to quit)"
  loop
  where
    loop = do
      eof <- hIsEOF stdin
      if eof
        then pure ()
        else do
          filePath <- getLine
          mainJob filePath
          loop

  