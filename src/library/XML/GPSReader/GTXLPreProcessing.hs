module XML.GPSReader.GTXLPreProcessing
  ( processTypeGraph
  , processRuleGraph
  , ElementCondition (..)
  , NodeId
  , NodeTypeId
  , EdgeId
  , EdgeTypeId
  , EdgeType
  , ProcessedNode
  , ProcessedEdge
  , ProcessedTypeGraph
  ) where

import           Data.List                 (isPrefixOf, partition)

import           XML.GPSReader.GTXLParseIn

data ElementCondition = Creation | Deletion | Preservation | Forbidden

type NodeId = Id
type NodeTypeId = Id
type EdgeId = Id
type EdgeTypeId = Id

type NodeType = String
type EdgeType = String
type NodeWithTypeId = (NodeType,Id)

-- rule types
type ProcessedNode = (NodeId,NodeTypeId,ElementCondition)
type ProcessedEdge = (EdgeTypeId,NodeId,NodeId,EdgeTypeId,ElementCondition)
type ProcessedRuleGraph = ([ProcessedNode],[ProcessedEdge])

-- type graph types
type TypeGraphNode = NodeWithTypeId
type TypeGraphEdge = (Id, Id, Label, EdgeTypeId)
type ProcessedTypeGraph = ([TypeGraphNode],[TypeGraphEdge])

specialLabels :: [String]
specialLabels = ["new:","del:","not:"]

ignoredLabels :: [String]
ignoredLabels = ["use:"]

normalizeLabel :: Label -> Label
normalizeLabel label = if ':' `elem` label then tail (dropWhile (/= ':') label) else label

-- process type graph
processTypeGraph :: ParsedRuleGraph -> ProcessedTypeGraph
processTypeGraph (_,(nodes,edges)) = processTypeGraphEdges nodes edges

processTypeGraphEdges :: [ParsedNode] -> [ParsedEdge] -> ProcessedTypeGraph
processTypeGraphEdges nodes = foldr (processTypeGraphEdge nodes) ([],[])

processTypeGraphEdge :: [ParsedNode] -> ParsedEdge -> ProcessedTypeGraph -> ProcessedTypeGraph
processTypeGraphEdge nodes ((src,tgt,label),id) =
  if "type:" `isPrefixOf` label
    then addFst (normalizeLabel label, idt src)
    else addSnd (idt src, idt tgt, label, id)
  where
    addFst z (x,y) = (z:x,y)
    addSnd z (x,y) = (x,z:y)
    idt name = head [nid | (node,nid) <- nodes, node == name]

processRuleGraph :: ProcessedTypeGraph -> ParsedRuleGraph -> ProcessedRuleGraph
processRuleGraph tg@(nodeTypes,_) rule = (processedNodes,processedEdges)
  where
    (nodes,edges) = snd rule
    
    removedIgnoredLabels = map adjustIgnoredLabels (filter removeIgnoredLabels edges)
    
    (ruleTyping,typeNo) = partition (\((_,_,label),_) -> isPrefixOf "type:" label) removedIgnoredLabels
    (nonPreservNodes,edgs) = partition (\((_,_,label),_) -> (label `elem` specialLabels)) typeNo
    
    processedNodes = processNodes nodeTypes ruleTyping nonPreservNodes nodes
    processedEdges = processEdges tg ruleTyping nodes nonPreservNodes edgs

removeIgnoredLabels :: ParsedEdge -> Bool
removeIgnoredLabels ((_,_,label),_) = label `notElem` ignoredLabels

adjustIgnoredLabels :: ParsedEdge -> ParsedEdge
adjustIgnoredLabels e@((src,tgt,label),id) =
  if any (`isPrefixOf` label) ignoredLabels
    then ((src, tgt, normalizeLabel label), id)
    else e

processEdges :: ProcessedTypeGraph -> [ParsedEdge] -> [NodeWithTypeId] -> [ParsedEdge] -> [ParsedEdge] -> [ProcessedEdge]
processEdges tg ruleTyping nodes nonPreservNodes = map (processEdge tg ruleTyping nodes nonPreservNodes)

processEdge :: ProcessedTypeGraph -> [ParsedEdge] -> [NodeWithTypeId] -> [ParsedEdge] -> ParsedEdge -> ProcessedEdge
processEdge (nodeTypes,edgeTypes) ruleTyping nodes nonPreservNodes ((nsrc,ntgt,label),id) =
  (id, idt nsrc, idt ntgt, edgeType, edgeCondition)
  where
    idt name = head [idn_ | (n,idn_) <- nodes, n == name]
    
    (edgeType:_) = [ide_ |
                     (src_,tgt_,lbl_,ide_) <- edgeTypes,
                     src_ == getTypeLabel nsrc,
                     tgt_ == getTypeLabel ntgt,
                     lbl_ == normalizeLabel label]
    
    getTypeLabel name = head [idt | (lbl,idt) <- nodeTypes, lbl == typeName]
      where
        typeName = head [normalizeLabel lbl | ((n,_,lbl),_) <- ruleTyping, n == name]
    
    edgeCondition
      | ':' `elem` label =
        case takeWhile (':' /=) label of
          "del" -> Deletion
          "new" -> Creation
          _      -> error "processEdges: edgeCondition invalid"
      | otherwise        = checkNacSrcTgt
    
    checkNacSrcTgt
      | srctgtCondition "not:" = Forbidden
      | srctgtCondition "del:" = Deletion
      | srctgtCondition "new:" = Creation
      | otherwise = Preservation
    
    srctgtCondition str = not (null [lbl | ((node,_,lbl),_) <- nonPreservNodes, lbl == str, node == nsrc || node == ntgt])

processNodes :: [NodeWithTypeId] -> [ParsedEdge] -> [ParsedEdge] -> [ParsedNode] -> [ProcessedNode]
processNodes types ruleTyping nonPreservNodes = map (processNode types ruleTyping nonPreservNodes)

processNode :: [NodeWithTypeId] -> [ParsedEdge] -> [ParsedEdge] -> ParsedNode -> ProcessedNode
processNode types ruleTyping nonPreservNodes (node,id) = (id, nodeType, cond)
  where
    (nodeTypeLabel:_) = [lbl | ((n,_,lbl),_) <- ruleTyping, n == node]
    (nodeType:_) = [idt | (name,idt) <- types, name == normalizeLabel nodeTypeLabel]
    
    c = [c_ | ((nt_,_,c_),_) <- nonPreservNodes, nt_ == node]
    cond
      | null c = Preservation
      | head c == "del:" = Deletion
      | head c == "new:" = Creation
      | head c == "not:" = Forbidden
      | otherwise = error ("processNodes: " ++ show c)
