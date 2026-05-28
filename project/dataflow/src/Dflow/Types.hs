module Dflow.Types
(Program(..), Node(..), Edge(..), Value(..))
 where

data Program = Program [Node] [Edge]

-- A node has a unique id, a kind ("source", "filter", ...) and parameters
data Node = Node
  { nodeId     :: String
  , nodeKind   :: String
  , nodeParams :: [(String, Value)]
  }

-- An edge connects one node's output to another node's input
data Edge = Edge { from :: String, to :: String }

data Value
  = StrVal  String
  | NumVal  Double
  | BoolVal Bool
  | ListVal [Value]