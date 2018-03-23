{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

-- |

module Category.TypedGraph.PartialMapClassifier () where


import           Abstract.Category.PartialMapClassifier
import           Category.TypedGraph
import qualified Data.Graphs                            as G
import qualified Data.Graphs.Morphism                   as GM
import           Data.Map                               (findWithDefault)
import           Data.TypedGraph
import           Data.TypedGraph.Morphism

import           Util.Map

instance PartialMapClassifier (TypedGraphMorphism a b) where

  totalizePartialMap = undefined

  partialMapClassifier :: TypedGraph a b -> TypedGraphMorphism a b
  partialMapClassifier typedGraph = undefined

data RenameState a b =
  RS {
    domainNodesWithTypes        :: [(Node (Maybe a), NodeId)]
  , renamedDomainNodesWithTypes :: [(NodeId, NodeId, Node (Maybe a), NodeId)]
  , domainEdgesWithTypes        :: [(Edge (Maybe b), EdgeId)]
  , renamedDomainEdgesWithTypes :: [(EdgeId, EdgeId, Edge (Maybe b), EdgeId)]

  , typeGraphNodes              :: [Node (Maybe a)]
  , renamedTypeGraphNodes       :: [(NodeId, NodeId, Node (Maybe a), NodeId)]

  , newNodeIdState              :: NodeId
  , newEdgeIdState              :: EdgeId
  }

buildInclusionOfAppointedGraph :: TypedGraph a b -> TypedGraph a b -> RenameState a b -> TypedGraphMorphism a b
buildInclusionOfAppointedGraph typedGraph appointedTypedGraph state = buildTypedGraphMorphism typedGraph appointedTypedGraph inclusion
  where
    nodesTypedGraph = nodeIds typedGraph
    edgesTypedGraph = edgeIds typedGraph

    nodesInclusion = map (\oldNodeId ->
                             (oldNodeId, lookupRenamedNodesAndEdges oldNodeId (renamedDomainNodesWithTypes state))) nodesTypedGraph
    edgesInclusion = map (\oldEdgeId ->
                             (oldEdgeId, lookupRenamedNodesAndEdges oldEdgeId (renamedDomainEdgesWithTypes state))) edgesTypedGraph

    inclusion = GM.fromGraphsAndLists (toUntypedGraph typedGraph) (toUntypedGraph appointedTypedGraph) nodesInclusion edgesInclusion

addAppointedEdgesToTypedGraph :: TypedGraph a b -> TypedGraph a b
addAppointedEdgesToTypedGraph typedGraph =
  foldl (\tGraph (typeId, srcId, tgtId) ->
            GM.createEdgeOnDomain (head $ newEdges tGraph) srcId tgtId typeId tGraph) typedGraph edgesToCreate
  where
    typedGraphNodes = typedNodeIds typedGraph
    nodesByType = inverse typedGraphNodes
    typeEdges = G.edges $ typeGraph typedGraph
    edgesToCreate =
      do
        e <- typeEdges
        (edgeId e,,) <$> findWithDefault [] (sourceId e) nodesByType <*> findWithDefault [] (targetId e) nodesByType


buildTypedGraphFromRenamedUnion :: TypedGraph a b -> RenameState a b -> TypedGraph a b
buildTypedGraphFromRenamedUnion typedGraph renamedUnion = fromNodesAndEdges (typeGraph typedGraph) nodesUnion edgesUnion
  where
    renamedNodesFromDomain = renamedDomainNodesWithTypes renamedUnion
    renamedEdgesFromDomain = renamedDomainEdgesWithTypes renamedUnion
    nodesUnion = map (\(_,_,x,y) -> (x,y)) (renamedNodesFromDomain ++ renamedTypeGraphNodes renamedUnion)
    edgesUnion = map (\(_,_,x,y) -> (x,y)) renamedEdgesFromDomain


buildRenamedUnionState :: TypedGraph a b -> RenameState a b
buildRenamedUnionState typedGraph = renameDomainEdges . renameTypeGraphNodes . renameDomainNodes $ initialRenameDomainState
  where
    initialRenameDomainState = RS (nodes typedGraph) [] (edges typedGraph) [] (G.nodes $ typeGraph typedGraph) [] 1 1

    renameDomainNodes :: RenameState a b -> RenameState a b
    renameDomainNodes state =
      case state of
        RS [] _ _ _ _ _ _ _ ->
          state

        RS ((node,nodeType):nodesToRename) renamedNodes _ _ _ _ newNodeId _ ->
          renameDomainNodes state { domainNodesWithTypes = nodesToRename
                                  , renamedDomainNodesWithTypes =
                                    (oldNodeId, newNodeId, renamedNode, nodeType) : renamedNodes
                                  , newNodeIdState = newNodeId + 1
                                  }
          where
            oldNodeId = nodeId node
            renamedNode = node { nodeId = newNodeId }


    renameDomainEdges :: RenameState a b -> RenameState a b
    renameDomainEdges state =
      case state of
        RS _ _ [] _ _ _ _ _ ->
          state

        RS _ renamedNodes ((edge,edgeType):edgesToRename) renamedEdges _ _ _ newEdgeId ->
          renameDomainEdges state { domainEdgesWithTypes = edgesToRename
                                  , renamedDomainEdgesWithTypes =
                                    (oldEdgeId, newEdgeId, renamedEdge, edgeType) : renamedEdges
                                  , newEdgeIdState = newEdgeId + 1
                                  }
          where
            oldEdgeId = edgeId edge
            renamedEdge = edge { edgeId = newEdgeId
                               , sourceId = lookupRenamedNodesAndEdges (sourceId edge) renamedNodes
                               , targetId = lookupRenamedNodesAndEdges (targetId edge) renamedNodes
                               }


    renameTypeGraphNodes :: RenameState a b -> RenameState a b
    renameTypeGraphNodes state =
      case state of
        RS _ _ _ _ [] _ _ _ ->
          state

        RS _ _ _ _ (node:nodesToRename) renamedNodes newNodeId _ ->
          renameTypeGraphNodes state { typeGraphNodes = nodesToRename
                                     , renamedTypeGraphNodes =
                                       (oldNodeId, newNodeId, renamedNode, oldNodeId) : renamedNodes
                                     , newNodeIdState = newNodeId + 1
                                     }
          where
            oldNodeId = nodeId node
            renamedNode = node { nodeId = newNodeId }


lookupRenamedNodesAndEdges :: Eq a => a -> [(a, a, b, a)] -> a
lookupRenamedNodesAndEdges _ [] = error "Node not renamed"
lookupRenamedNodesAndEdges lookupNodeId ((oldName, newName, _ , _):t) =
  if lookupNodeId == oldName then newName else lookupRenamedNodesAndEdges lookupNodeId t

