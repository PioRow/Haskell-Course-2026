module Dflow.Types
(Program(..), Node(..), Edge(..), Value(..),PhotoObj(..)
,getEdges, getNodes)
 where
import qualified Data.Map as M
getEdges :: Program -> [Edge]
getEdges (Program _ es) = es
getNodes :: Program -> [Node]
getNodes (Program ns _) = ns

data Program = Program [Node] [Edge] deriving (Show, Eq)
data Value
  = StrVal  String
  | NumVal  Double
  | BoolVal Bool
  | ListVal [Value] 
  deriving (Show, Eq)
-- A node has a unique id, a kind ("source", "filter", ...) and parameters
data Node = Node
  { nodeId     :: String
  , nodeKind   :: String
  , nodeParams :: [(String, Value)]
  } deriving (Show, Eq)

-- An edge connects one node's output to another node's input
data Edge = Edge { from :: String, to :: String }
  deriving (Show, Eq)


data  PhotoObj = PhotoObj { getFields :: M.Map String Value } deriving (Show, Eq)
