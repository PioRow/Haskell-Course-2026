module Dflow.Executor (
    execute
) where
import Control.Monad (foldM)
import Dflow.Types
import qualified Data.Map as M
validFields :: [String]
validFields = [ "name", "width", "height", "format", "filter", "tagged"]

updatePhotoFields :: M.Map String Value -> [(String, Value)] -> M.Map String Value
updatePhotoFields currentFields parameters =
    foldl applyOverride currentFields validFields
  where
    applyOverride accMap key =
      case lookup key parameters of
        Just newValue -> M.insert key newValue accMap 
        Nothing       -> accMap

executeSource :: [(String, Value)] -> Either String PhotoObj
executeSource params =
    let initialFields = updatePhotoFields (M.insert "Id" (StrVal "photo-id-gen") M.empty) params
 
    in Right (PhotoObj initialFields)

executeTransform :: PhotoObj -> [(String, Value)] -> Either String PhotoObj
executeTransform (PhotoObj currentFields) params =
    let transformedFields = updatePhotoFields currentFields params
    in Right (PhotoObj transformedFields)


executeSink :: PhotoObj -> [(String, Value)] -> Either String String
executeSink (PhotoObj currentFields) params =
    let finalFields = updatePhotoFields currentFields params
        
        rows   = [ key ++ ": " ++ show val | key <- validFields, Just val <- [M.lookup key finalFields] ]
    in Right (unlines rows)

type Environment = M.Map String PhotoObj


findParent :: String -> [Edge] -> Maybe String
findParent currentId edges =
    case [from e | e <- edges, to e == currentId] of
        [] -> Nothing
        (p:_) -> Just p

processNode ::[Edge]-> (Environment, [String]) -> Node -> Either String (Environment, [String])
processNode edges (env, sinkResults) node =
    case nodeKind node of
        "source" -> do
            photoObj <- executeSource (nodeParams node)
            let updatedEnv = M.insert (nodeId node) photoObj env
            Right (updatedEnv, sinkResults)
        "transform" -> do
            let maybeInputId = findParent (nodeId node) edges
            case maybeInputId of
                Nothing -> Left $ "Transform node '" ++ nodeId node ++ "' has no input edge."
                Just inputId -> case M.lookup inputId env of
                    Nothing -> Left $ "Transform node '" ++ nodeId node ++ "' cannot find input PhotoObj with id '" ++ inputId ++ "'."
                    Just inputPhotoObj -> do
                        transformedPhotoObj <- executeTransform inputPhotoObj (nodeParams node)
                        let updatedEnv = M.insert (nodeId node) transformedPhotoObj env
                        Right (updatedEnv, sinkResults)
        "sink" -> do
            let maybeInputId = findParent (nodeId node) edges
            case maybeInputId of
                Nothing -> Left $ "Sink node '" ++ nodeId node ++ "' has no input edge."
                Just inputId -> case M.lookup inputId env of
                    Nothing -> Left $ "Sink node '" ++ nodeId node ++ "' cannot find input PhotoObj with id '" ++ inputId ++ "'."
                    Just inputPhotoObj -> do
                        result <- executeSink inputPhotoObj (nodeParams node)
                        
                        Right (env, ((nodeId node ++ ":\n " ++ result++"\n") : sinkResults))
        _ -> Left $ "Unknown node kind: " ++ nodeKind node

execute:: Program -> Either String String
execute prog =
    let
        sortedNodes= getNodes prog
        edges = getEdges prog
    in 
    do
        (_,sinkResults) <- foldM (processNode edges) (M.empty, []) sortedNodes
        if null sinkResults
          then Right "Execution completed: No active sink outputs flushed."
          else Right (unlines (reverse sinkResults))
    