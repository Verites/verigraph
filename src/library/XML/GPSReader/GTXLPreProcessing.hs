module XML.GPSReader.GTXLPreProcessing where

import           Data.Maybe                    (fromMaybe)

import           XML.GPSReader.GTXLParseIn

data ElementCondition = Creation | Deletion | Preservation deriving(Eq,Show)

type ProcessedNode = (Id,Id,ElementCondition)
type ProcessedEdge = (Id,Id,Id,Id,ElementCondition)

nodeType :: Id
nodeType = 100

edgeType :: Id
edgeType = 100

getNodeId :: [NodeWithId] -> Node -> Id
getNodeId nodes node = fromIntegral (fromMaybe (error "node?") (lookup node nodes))

processEdges :: [NodeWithId] -> [EdgeWithId] -> [ProcessedEdge]
processEdges _ [] = []
processEdges nodes (((from,to,label),id):directedEdges) =
  (id, sourceId, targetId, edgeType, verify label) : processEdges nodes directedEdges
  where
    sourceId = getNodeId nodes from
    targetId = getNodeId nodes to
    verify ('n':'e':'w':':':_) = Creation
    verify ('d':'e':'l':':':_) = Deletion
    verify _ = Preservation

processNodes :: [NodeWithId] -> [EdgeWithId] -> [EdgeWithId] -> [ProcessedNode]
processNodes = processPreservedNodes

processCreatedDeletedNodes :: [NodeWithId] -> [EdgeWithId] -> [ProcessedNode]
processCreatedDeletedNodes _ [] = []  
processCreatedDeletedNodes nodes (((node,_,label),_):ruleEdges) =
  (nodeId, nodeType, verify label) : processCreatedDeletedNodes nodes ruleEdges
  where
    nodeId = getNodeId nodes node
    verify "new:" = Creation
    verify "del:" = Deletion
    verify er = error ("verifying unknown token: " ++ er)

processPreservedNodes :: [NodeWithId] -> [EdgeWithId] -> [EdgeWithId] -> [ProcessedNode]
processPreservedNodes nodes ruleEdges [] = processCreatedDeletedNodes nodes ruleEdges
processPreservedNodes nodes ruleEdges (((node,_,_),_):typingEdges) =
  if nodeWasCreatedDeleted
    then processPreservedNodes nodes ruleEdges typingEdges
    else (nodeId, nodeType, Preservation) : processPreservedNodes nodes ruleEdges typingEdges
  where
    nodeId = getNodeId nodes node
    nodeWasCreatedDeleted = Prelude.not (Prelude.null [x | x@((n2,_,_),_) <- ruleEdges, n2 == node])
