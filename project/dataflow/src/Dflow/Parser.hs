{-# LANGUAGE OverloadedStrings #-}
module Dflow.Parser (
  readFileAsText
) where

import Dflow.Types
-- Import your parsing library of choice (e.g., Megaparsec)
import Data.Void
import Data.Text as T
import Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

readFileAsText :: FilePath -> IO T.Text
readFileAsText = TIO.readFile

type Parser = Parsec Void T.Text
sc :: Parser ()
sc = L.space 
  space1 
  (L.skipLineComment "//") 
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc


