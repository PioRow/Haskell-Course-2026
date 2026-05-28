{-# LANGUAGE OverloadedStrings #-}
module Dflow.Parser (
  readFileAsText
) where

import Dflow.Types
import Data.Void
import Data.Text as T
import Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (Lexeme(Ident))

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
nodeKindP :: Parser String
nodeKindP = choice
  [ T.unpack <$> symbol "source"
  , T.unpack <$> symbol "transform"
  , T.unpack <$> symbol "sink"
  ]

identifierP :: Parser String
identifierP = lexeme $ (:) <$> letterChar <*> many alphaNumChar


paramsP :: Parser [(String, Value)]


type ParsedNode = (Node,Maybe Edge)

nodeP :: Parser ParsedNode
nodeP = do
  nodeId <- identifierP
  kind <- nodeKindP
  src <- optional $ symbol "from" *>identifierP
  params<- paramsP

