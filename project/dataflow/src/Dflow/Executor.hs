module Dflow.Executor (
    execute
) where
import Control.Monad (foldM)
import Dflow.Types
import qualified Data.Map as M
validFields :: [String]
validFields = [ "name", "width", "height", "format", "filter", "tagged"]

valueTypeName :: Value -> String
valueTypeName (StrVal _)  = "String"
valueTypeName (NumVal _)  = "Number"
valueTypeName (BoolVal _) = "Boolean"
valueTypeName (ListVal _) = "List"


updatePhotoFields :: M.Map String Value -> [(String, Value)] -> Either String (M.Map String Value)
updatePhotoFields currentFields parameters =
    foldM applyOverride currentFields validFields
  where
    applyOverride accMap key =
      case lookup key parameters of
        Just newValue -> 
            let oldValueType = fmap valueTypeName (M.lookup key currentFields)
                newValueType = valueTypeName newValue
            in if Just newValueType == oldValueType || oldValueType == Nothing
                then  Right(M.insert key newValue accMap)
            else Left $ "Incompatible field type for field " ++ key
        Nothing -> Right accMap

executeSource :: [(String, Value)] -> Either String PhotoObj
executeSource params =
    let initialFields =M.fromList [("id",StrVal "rnd-id-gen")]
    in
    case updatePhotoFields initialFields params of
        Right updatedFields -> Right (PhotoObj updatedFields)
        Left err -> Left $ "Source execution error: " ++ err

executeTransform :: PhotoObj -> [(String, Value)] -> Either String PhotoObj
executeTransform (PhotoObj currentFields) params =
    case updatePhotoFields currentFields params of
        Right updatedFields -> Right (PhotoObj updatedFields)
        Left err -> Left $ "transform execution error: " ++ err


executeSink :: PhotoObj -> [(String, Value)] -> Either String PhotoObj
executeSink (PhotoObj currentFields) params =
    case updatePhotoFields currentFields params of
        Right updatedFields -> Right (PhotoObj updatedFields)
        Left err -> Left $ "Sink execution error: " ++ err

type Environment = M.Map String PhotoObj


findParent :: String -> [Edge] -> Maybe String
findParent currentId edges =
    case [from e | e <- edges, to e == currentId] of
        [] -> Nothing
        (p:_) -> Just p

processNode ::[Edge]-> (Environment, [PhotoObj]) -> Node -> Either String (Environment, [PhotoObj])
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
                        finalPhotoObj <- executeSink inputPhotoObj (nodeParams node)
                        Right (env, sinkResults ++ [finalPhotoObj])
        _ -> Left $ "Unknown node kind: " ++ nodeKind node

execute:: Program -> Either String [PhotoObj]
execute prog =
    let
        sortedNodes= getNodes prog
        edges = getEdges prog
    in 
    do
        (_,sinkResults) <- foldM (processNode edges) (M.empty, []) sortedNodes
        if null sinkResults
          then Left "No sink nodes found in the program."
          else Right sinkResults
    