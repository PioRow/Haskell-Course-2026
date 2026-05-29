module Main (main) where

import qualified Data.Text.IO as TIO
import Dflow (programP,readFileAsText)

import Text.Megaparsec 
import System.Environment (getArgs)
import System.IO (hIsEOF, stdin)


mainJob :: String -> IO ()
mainJob filePath = do
    input <- readFileAsText filePath
    case parse programP filePath input of
      Left err -> putStrLn $ errorBundlePretty err
      Right prog -> do
        putStrLn "Parsed program:"
        print prog
    

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

  