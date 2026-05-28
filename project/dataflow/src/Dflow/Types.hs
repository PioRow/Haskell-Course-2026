module Dflow.Types
(Program(..), Node(..), Edge(..), Value(..))
 where



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

