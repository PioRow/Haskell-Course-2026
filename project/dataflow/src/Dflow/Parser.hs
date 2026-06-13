{-# LANGUAGE OverloadedStrings #-}
module Dflow.Parser (
  readFileAsText
  , programP
  
) where

import Dflow.Types
import Data.Void
import Data.Text as T
import Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as S

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

srcNodeKindP :: Parser String
srcNodeKindP= T.unpack <$> symbol "source"
nodeKindP :: Parser String
nodeKindP = choice
  [ T.unpack <$> symbol "transform"
  , T.unpack <$> symbol "sink"
  ]


reservedKeywords :: S.Set String
reservedKeywords = S.fromList ["source", "transform", "sink", "from", "true", "false"]

identifierP :: Parser String
identifierP = lexeme $ do
  name <- (:) <$> letterChar <*> many alphaNumChar
  if name `S.member` reservedKeywords
    then fail $ "Identifier cannot be a reserved keyword: " ++ name
    else return name



stringP :: Parser Value
stringP = StrVal <$> (char '"' *> manyTill L.charLiteral (char '"'))

numberP :: Parser Value
numberP = lexeme $ (NumVal <$> try L.float) <|> (NumVal . fromIntegral <$> (L.decimal :: Parser Integer))
boolP :: Parser Value
boolP = (BoolVal True <$ symbol "true") <|> (BoolVal False <$ symbol "false")

primValueP :: Parser Value
primValueP =  lexeme $ stringP <|> numberP <|> boolP

listP :: Parser Value
listP = ListVal <$> between (symbol "[") (symbol "]") (primValueP `sepEndBy` symbol ",") 

valueP :: Parser Value
valueP = lexeme $ primValueP <|> listP
pairP :: Parser (String, Value)
pairP= do
  key <- identifierP
  _ <- symbol ":"
  val <- valueP
  return (key, val)



paramsP :: Parser [(String, Value)]
paramsP= lexeme $ between (symbol "{") (symbol "}") (pairP `sepEndBy` symbol ",")


srcP :: Parser (String,String, Maybe String)
srcP = do
  kind <- srcNodeKindP
  nodeIdent <- identifierP
  return (kind, nodeIdent, Nothing)
nonSrcP :: Parser (String,String, Maybe String)
nonSrcP = do
  kind <- nodeKindP
  nodeIdent <- identifierP
  _ <- symbol "from"
  srcIdent <- identifierP
  return (kind, nodeIdent, Just srcIdent)

nodeHeaderP :: Parser (String,String, Maybe String)
nodeHeaderP = try nonSrcP <|> srcP
  

type ParsedNode = (Node,Maybe Edge)

nodeP :: Parser ParsedNode
nodeP = do
  (kind, nodeIdent, src) <- nodeHeaderP
  params<- paramsP
  case src of
    Just s -> return (Node nodeIdent kind params, Just (Edge s nodeIdent))
    Nothing -> return (Node nodeIdent kind params, Nothing)

programP :: Parser Program
programP = do
  sc
  nodesWithEdges <- many nodeP
  eof
  let (nodes, edges) = Prelude.foldr repack ([],[]) nodesWithEdges
  return $ Program nodes edges
  where
    repack :: ParsedNode -> ([Node], [Edge]) -> ([Node], [Edge])
    repack (n, Just e) (ns, es) = (n:ns, e:es)
    repack (n, Nothing) (ns, es) = (n:ns, es)