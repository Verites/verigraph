{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Data.LabeledGraph.Morphism
  (
  -- * Morphism type
    LabeledMorphism(..)

  -- * Query
  , isTotal
  , isTotalOnNodes
  , isTotalOnEdges
  , isTotalOnVariables
  , isInjective
  , isSurjective
  , isBijective
  , applyToNode
  , applyToEdge
  , applyToVariable
  , applyToNodeId
  , applyToEdgeId
  , lookupNodeId
  , lookupEdgeId
  , isNodeDefined
  , isEdgeDefined

  -- * Construction
  , fromGraphsAndLists
  , compose

  -- * Conversion
  -- ** Maps
  -- $conversion-maps

  -- ** Relations
  , nodeRelation
  , edgeRelation
  , variableRelation

  -- ** Lists
  , orphanNodes
  , orphanEdges
  , orphanVariables
  ) where


import           Data.Foldable     (foldl')
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe        (isJust, maybeToList)
import qualified Data.Set          as Set
import qualified Data.Text         as Text

import           Base.Valid
import           Data.EnumMap      (EnumMap)
import qualified Data.EnumMap      as EnumMap
import qualified Data.EnumSet      as EnumSet
import           Data.LabeledGraph hiding (edgeMap, nodeMap)
import           Data.Relation     (Relation)
import qualified Data.Relation     as Relation
import           Data.Variable


data LabeledMorphism = LabeledMorphism
  { domainGraph   :: LabeledGraph
  , codomainGraph :: LabeledGraph
  , nodeMap       :: EnumMap NodeId NodeId
  , edgeMap       :: EnumMap EdgeId EdgeId
  , variableMap   :: Map Variable Variable
  }
  deriving Eq


instance Show LabeledMorphism where

  show m = concat
    [ "LNode mappings:\n"
    , concatMap showNodeMapping (EnumMap.toList $ nodeMap m)
    , "LEdge mappings:\n"
    , concatMap showEdgeMapping (EnumMap.toList $ edgeMap m)
    , "Variable renamings:\n"
    , concatMap showVarRenaming (Map.toList $ variableMap m) ]
    where
      showNodeMapping (id1, id2) = "\t" ++ showNode id1 (domainGraph m) ++ " => " ++ showNode id2 (codomainGraph m) ++ "\n"
      showNode n graph = case lookupNode n graph of
        Nothing -> "INVALID(" ++ show n ++ ")"
        Just node -> show n ++ " [" ++ label node ++ "]"
        where label = maybe "" Text.unpack . nodeLabel
      showEdgeMapping (id1, id2) = "\t" ++ showEdge id1 (domainGraph m) ++ " => " ++ showEdge id2 (codomainGraph m) ++ "\n"
      showEdge e graph = case lookupEdge e graph of
        Nothing -> "INVALID(" ++ show e ++ ")"
        Just edge -> show e ++ " [" ++ show (sourceId edge) ++ "->" ++ show (targetId edge) ++ "]"
      showVarRenaming (v1, v2) = Text.unpack v1 ++ " => " ++ Text.unpack v2 ++ "\n"


instance Valid LabeledMorphism where

  validate morphism = mconcat
    [ withContext "domain" $ validate (domainGraph morphism)
    , withContext "codomain" $ validate (codomainGraph morphism)
    , ensure (isTotalOnNodes morphism) "the morphism is not total on nodes"
    , ensure (isTotalOnEdges morphism) "the morphism is not total on edges"
    , ensure (isTotalOnVariables morphism) "the morphism is not total on variables"
    , ensure preservesIncidence "the morphism doesn't preserve incidence/adjacency"
    , ensure preservesLabeling "the morphism doesn't preserve labeling"
    ]
    where
      preservesIncidence = all preservedAtEdge (edges $ domainGraph morphism)
        where
          preservedAtEdge edge =
            (sourceId <$> applyToEdge edge morphism) == lookupNodeId (sourceId edge) morphism
            && (targetId <$> applyToEdge edge morphism) == lookupNodeId (targetId edge) morphism
      preservesLabeling = all preservedAtNode (nodes $ domainGraph morphism)
        where
          preservedAtNode node =
            case (nodeLabel node, nodeLabel =<< applyToNode node morphism) of
              (Nothing, _)      -> True
              (Just _, Nothing) -> False
              (Just v, justW)   -> applyToVariable v morphism == justW


-- * Query

isTotal, isTotalOnNodes, isTotalOnEdges, isTotalOnVariables :: LabeledMorphism -> Bool
isTotal m = isTotalOnNodes m && isTotalOnEdges m && isTotalOnVariables m

isTotalOnNodes m = EnumMap.keysSet (nodeMap m) == (EnumSet.fromList . nodeIds $ domainGraph m)
isTotalOnEdges m = EnumMap.keysSet (edgeMap m) == (EnumSet.fromList . edgeIds $ domainGraph m)
isTotalOnVariables m = Map.keysSet (variableMap m) == freeVariableSet (domainGraph m)

isInjective :: LabeledMorphism -> Bool
isInjective m =
  isInjective' (nodeMap m) && isInjective' (edgeMap m) && isInjective' (variableMap m)
  where
    isInjective' = all (<=1) . preimageCounts
    preimageCounts = foldl' incrementCount Map.empty
    incrementCount counts x = Map.insertWith (+) x (1 :: Int) counts

isSurjective :: LabeledMorphism -> Bool
isSurjective m =
    enumMapRange (nodeMap m) == (EnumSet.fromList . nodeIds $ codomainGraph m)
      && enumMapRange (edgeMap m) == (EnumSet.fromList . edgeIds $ codomainGraph m)
      && mapRange (variableMap m) == freeVariableSet (codomainGraph m)
    where
      enumMapRange = EnumSet.fromList . EnumMap.elems
      mapRange = Set.fromList . Map.elems

isBijective :: LabeledMorphism -> Bool
isBijective m = isInjective m && isSurjective m

applyToNode :: LNode -> LabeledMorphism -> Maybe LNode
applyToNode = applyToNodeId . nodeId

applyToEdge :: LEdge -> LabeledMorphism -> Maybe LEdge
applyToEdge = applyToEdgeId . edgeId

applyToNodeId :: NodeId -> LabeledMorphism -> Maybe LNode
applyToNodeId nodeId f = do
  nodeId' <- lookupNodeId nodeId f
  lookupNode nodeId' (codomainGraph f)

applyToEdgeId :: EdgeId -> LabeledMorphism -> Maybe LEdge
applyToEdgeId edgeId f = do
  edgeId' <- lookupEdgeId edgeId f
  lookupEdge edgeId' (codomainGraph f)

applyToVariable :: Variable -> LabeledMorphism -> Maybe Variable
applyToVariable var = Map.lookup var . variableMap

lookupNodeId :: NodeId -> LabeledMorphism -> Maybe NodeId
lookupNodeId nodeId = EnumMap.lookup nodeId . nodeMap

lookupEdgeId :: EdgeId -> LabeledMorphism -> Maybe EdgeId
lookupEdgeId edgeId = EnumMap.lookup edgeId . edgeMap

isNodeDefined :: NodeId -> LabeledMorphism -> Bool
isNodeDefined nodeId m = isJust (applyToNodeId nodeId m)

isEdgeDefined :: EdgeId -> LabeledMorphism -> Bool
isEdgeDefined edgeId m = isJust (applyToEdgeId edgeId m)


-- * Construction

-- | Creates a morphism between the given labeled graphs, mapping nodes and edges according to the
-- given lists. The variable renaming is constructed by first ensuring consistency with the node
-- mapping and attribution, then mapping additional variables according to the given list.
--
-- WARNING: the result could be an invalid morphism for any of the following reasons:
--  * Some node id, edge id or variable isn't part of the appropriate graph
--  * Incidence is not preserved (i.e. the source/target of an edge is mapped to a node that is not
--    the source/target of the mapped edge)
--  * Labeling is not preserved (i.e. either a node with defined label is mapped to a node with
--    undefined label, or the label of a node in the domainGraph is not mapped to the label of the
--    corresponding node in the codomainGraph)
fromGraphsAndLists
  :: LabeledGraph -> LabeledGraph
      -> [(NodeId, NodeId)] -> [(EdgeId, EdgeId)] -> [(Variable, Variable)]
      -> LabeledMorphism
fromGraphsAndLists dom cod nodeMap edgeMap varMap = LabeledMorphism
  { domainGraph = dom
  , codomainGraph = cod
  , nodeMap = EnumMap.fromList nodeMap
  , edgeMap = EnumMap.fromList edgeMap
  , variableMap = inducedRenaming `Map.union` Map.fromList varMap
  }
  where
    inducedRenaming = Map.fromList
      [ (v1, v2)
          | (n1, n2) <- nodeMap
          , v1 <- maybeToList $ nodeLabel =<< lookupNode n1 dom
          , v2 <- maybeToList $ nodeLabel =<< lookupNode n2 cod
      ]

-- | Given @f : A -> B@ and @g : B -> C@, @compose g f : A -> C@ is a graph morphism equivalent to
-- successively applying @f@, then @g@.
compose :: LabeledMorphism -> LabeledMorphism -> LabeledMorphism
compose g f = LabeledMorphism
  { domainGraph = domainGraph f
  , codomainGraph = codomainGraph g
  , nodeMap = composeEnumMaps (nodeMap f) (nodeMap g)
  , edgeMap = composeEnumMaps (edgeMap f) (edgeMap g)
  , variableMap = composeMaps (variableMap f) (variableMap g)
  }
  where
    composeEnumMaps firstMap secondMap = EnumMap.fromList
      [ (x, z)
          | (x, y) <- EnumMap.toList firstMap
          , Just z <- [EnumMap.lookup y secondMap] ]
    composeMaps firstMap secondMap = Map.fromList
        [ (x, z)
            | (x, y) <- Map.toList firstMap
            , Just z <- [Map.lookup y secondMap] ]
{-# INLINE compose #-}


-- * Conversion
-- $conversion-maps
-- In order to extract the maps from a morphism, use 'nodeMap', 'edgeMap' and 'variableMap'.

-- ** Relations

nodeRelation :: LabeledMorphism -> Relation NodeId
nodeRelation f =
  Relation.fromPairs (nodeIds $ domainGraph f) (nodeIds $ codomainGraph f) (EnumMap.toList $ nodeMap f)

edgeRelation :: LabeledMorphism -> Relation EdgeId
edgeRelation f =
  Relation.fromPairs (edgeIds $ domainGraph f) (edgeIds $ codomainGraph f) (EnumMap.toList $ edgeMap f)

variableRelation :: LabeledMorphism -> Relation Variable
variableRelation f = Relation.fromPairs
  (freeVariablesOf $ domainGraph f)
  (freeVariablesOf $ codomainGraph f)
  (Map.toList $ variableMap f)


-- ** Lists

orphanNodes :: LabeledMorphism -> [NodeId]
orphanNodes f = EnumSet.toList orphans
  where
    orphans = EnumSet.difference
      (EnumSet.fromList . nodeIds $ domainGraph f)
      (EnumMap.keysSet $ nodeMap f)

orphanEdges :: LabeledMorphism -> [EdgeId]
orphanEdges f = EnumSet.toList orphans
  where
    orphans = EnumSet.difference
      (EnumSet.fromList . edgeIds $ domainGraph f)
      (EnumMap.keysSet $ edgeMap f)

orphanVariables :: LabeledMorphism -> [Variable]
orphanVariables f = Set.toList $
  Set.difference
    (freeVariableSet $ domainGraph f)
    (Map.keysSet $ variableMap f)
