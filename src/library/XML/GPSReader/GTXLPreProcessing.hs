module XML.GPSReader.GTXLPreProcessing where

import           Data.List
import           Data.Maybe                    (fromMaybe)

import           XML.GPSReader.GTXLParseIn

data ElementCondition = Creation | Deletion | Preservation | Forbidden deriving(Eq,Show)

type NodeId = Id
type NodeTypeId = Id
type EdgeId = Id
type EdgeTypeId = Id

type ProcessedNode = (NodeId,NodeTypeId,ElementCondition)
type ProcessedEdge = (EdgeTypeId,NodeId,NodeId,EdgeTypeId,ElementCondition)

type NodeType = String
type EdgeType = String
type NodeWithTypeId = (NodeType,Id)
type EdgeWithTypeId = (EdgeType,Id)
type GraphTypes = ([NodeWithTypeId],[EdgeWithTypeId])

specialWords :: [String]
specialWords = ["new:","del:","not:"]

getNodeId :: [NodeWithId] -> Node -> Id
getNodeId nodes node =
  fromMaybe
    (error ("getNodeId error: " ++ show node ++ " -> " ++ show nodes))
    (lookup node nodes)

normalizeLabel :: Label -> Label
normalizeLabel label = if ':' `elem` label then tail (dropWhile (/= ':') label) else label

-- process type graph
processTypeGraph :: NamedRuleGraph -> GraphTypes
processTypeGraph (_,(_,edges)) = processTypeGraphEdges edges

processTypeGraphEdges :: [EdgeWithId] -> GraphTypes
processTypeGraphEdges [] = ([],[])
processTypeGraphEdges (((_,_,label),id):edges) =
  (if "type:" `isPrefixOf` label
    then addFst (normalizeLabel label,id)
    else addSnd (label,id)
  )
  (processTypeGraphEdges edges)
  where
    addFst z (x,y) = (z:x,y)
    addSnd z (x,y) = (x,z:y)

processRuleGraph :: GraphTypes -> NamedRuleGraph -> ([ProcessedNode],[ProcessedEdge])
processRuleGraph (nodeTypes,edgeTypes) rule = (processedNodes, processedEdges)
  where
    (nodes,edges) = snd rule
    (typeYes,typeNo) = partition (\((_,_,label),_) -> isPrefixOf "type:" label) edges
    (nonPreservNodes,edgs) = partition (\((_,_,label),_) -> (label `elem` specialWords)) typeNo
    
    processedNodes = processNodes nodeTypes typeYes nonPreservNodes nodes
    processedEdges = processEdges edgeTypes nodes nonPreservNodes edgs

processEdges :: [EdgeWithTypeId] -> [NodeWithId] -> [EdgeWithId] -> [EdgeWithId] -> [ProcessedEdge]
processEdges _ _ _ [] = []
processEdges edgeTypes nodes nonPreservNodes (((nsrc,ntgt,label),id):edges) =
  (id, srcId, tgtId, edgeType, cond) : processEdges edgeTypes nodes nonPreservNodes edges
  where
    (srcId:_) = [idn_ | (n,idn_) <- nodes, n == nsrc]
    (tgtId:_) = [idn_ | (n,idn_) <- nodes, n == ntgt]
    
    (edgeType:_) = [ide_ | (e,ide_) <- edgeTypes, e == normalizeLabel label]
    
    cond = if ':' `elem` label
             then (if takeWhile (':' /=) label == "del" then Deletion else Creation)
             else checkNacSrcTgt
    
    checkNacSrcTgt
      | srctgtForbidn = Forbidden
      | srctgtDeleted = Deletion
      | srctgtCreated = Creation
      | otherwise = Preservation
    
    srctgtForbidn = not (null [lbl | ((node,_,lbl),_) <- nonPreservNodes, lbl == "not:", node == nsrc || node == ntgt])
    srctgtDeleted = not (null [lbl | ((node,_,lbl),_) <- nonPreservNodes, lbl == "del:", node == nsrc || node == ntgt])
    srctgtCreated = not (null [lbl | ((node,_,lbl),_) <- nonPreservNodes, lbl == "new:", node == nsrc || node == ntgt])

processNodes :: [NodeWithTypeId] -> [EdgeWithId] -> [EdgeWithId] -> [NodeWithId] -> [ProcessedNode]
processNodes _ _ _ [] = []
processNodes types typeYes nonPreservNodes ((node,id):nodes) =
  (id, nodeType, cond) : processNodes types typeYes nonPreservNodes nodes
  where
    (nodeTypeLabel:_) = [lbl | ((n,_,lbl),_) <- typeYes, n == node]
    (nodeType:_) = [idt | (name,idt) <- types, name == normalizeLabel nodeTypeLabel]
    
    c = [c_ | ((nt_,_,c_),_) <- nonPreservNodes, nt_ == node]
    cond
      | null c = Preservation
      | head c == "del:" = Deletion
      | head c == "new:" = Creation
      | head c == "not:" = Forbidden
      | otherwise = error ("processNodes: " ++ show c)
