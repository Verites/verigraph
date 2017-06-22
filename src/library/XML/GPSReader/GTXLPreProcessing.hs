module XML.GPSReader.GTXLPreProcessing where

import           Data.List
import           Data.Maybe                    (fromMaybe)

import           XML.GPSReader.GTXLParseIn

data ElementCondition = Creation | Deletion | Preservation | Forbidden deriving(Eq,Show)

type ProcessedNode = (Id,Id,ElementCondition)
type ProcessedEdge = (Id,Id,Id,Id,ElementCondition)

getNodeId :: [NodeWithId] -> Node -> Id
getNodeId nodes node =
  fromIntegral
    (fromMaybe (error ("error\n" ++ show nodes ++" <-> " ++ show node))
    (lookup node nodes))

processTypeGraph :: [EdgeWithId] -> ([(String,Id)],[(String,Id)])
processTypeGraph [] = ([],[])
processTypeGraph (((_,_,label),id):edges) =
  (if "type:" `isPrefixOf` label
    then addFst (normalizeLabel label,id)
    else addSnd (label,id)
  )
  (processTypeGraph edges)
  where
    addFst z (x,y) = (z:x,y)
    addSnd z (x,y) = (x,z:y)

processEdges :: [NodeWithId] -> [NodeWithId] -> [EdgeWithId] -> [ProcessedEdge]
processEdges _ _ [] = []
processEdges types nodes (((from,to,label),id):directedEdges) =
  (id, sourceId, targetId, edgeType, verify label) : processEdges types nodes directedEdges
  where
    sourceId = getNodeId nodes from
    targetId = getNodeId nodes to
    edgeType = getNodeId types (normalizeLabel label)
    verify ('n':'e':'w':':':_) = Creation
    verify ('d':'e':'l':':':_) = Deletion
    verify ('n':'o':'t':':':_) = Forbidden
    verify _ = Preservation

processNodes :: [NodeWithId] -> [NodeWithId] -> [EdgeWithId] -> [EdgeWithId] -> [ProcessedNode]
processNodes types nodes ruleEdges typingEdges =
  processCreatedDeletedNodes types nodes typingEdges ruleEdges ++
  processPreservedNodes types nodes ruleEdges typingEdges

processCreatedDeletedNodes :: [NodeWithId] -> [NodeWithId] -> [EdgeWithId] -> [EdgeWithId] -> [ProcessedNode]
processCreatedDeletedNodes _ _ _ [] = []
processCreatedDeletedNodes types nodes typingEdges (((node,_,label),_):ruleEdges) =
  (nodeId, nodeType, verify label) : processCreatedDeletedNodes types nodes typingEdges ruleEdges
  where
    ((n2,_,l2),_) = head [x | x@((n,_,l),_) <- typingEdges, "type:" `isPrefixOf` l, n == node]
    
    nodeId = getNodeId nodes n2
    nodeType = getNodeId types (normalizeLabel l2)
    
    verify "new:" = Creation
    verify "del:" = Deletion
    verify "not:" = Forbidden
    verify msg = error (show msg) 

processPreservedNodes :: [NodeWithId] -> [NodeWithId] -> [EdgeWithId] -> [EdgeWithId] -> [ProcessedNode]
processPreservedNodes _ _ _ [] = []
processPreservedNodes types nodes ruleEdges (((node,_,label),_):typingEdges) =
  if nodeWasCreatedDeleted
    then processPreservedNodes types nodes ruleEdges typingEdges
    else (nodeId, nodeType, Preservation) : processPreservedNodes types nodes ruleEdges typingEdges
  where
    nodeId = getNodeId nodes node
    nodeType = getNodeId types (normalizeLabel label)
    nodeWasCreatedDeleted = Prelude.not (Prelude.null [x | x@((n2,_,_),_) <- ruleEdges, n2 == node])

normalizeLabel :: String -> String
normalizeLabel label = if ':' `elem` label then tail (dropWhile (/= ':') label) else label
