{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE TypeFamilies              #-}
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
  , applyToNodeId
  , applyToEdgeId
  , lookupNodeId
  , lookupEdgeId
  , lookupVarId
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
  ) where


import qualified Data.List         as List
import           Data.Maybe        (isJust)
import qualified Data.Text         as Text

import           Base.Valid
import           Data.EnumMap      (EnumMap)
import qualified Data.EnumMap      as EnumMap
import qualified Data.EnumSet      as EnumSet
import           Data.LabeledGraph hiding (edgeMap, nodeMap)
import           Data.Relation     (Relation)
import qualified Data.Relation     as Relation
import           Data.Variable
import qualified Util.EnumMap      as EnumMap


data LabeledMorphism = LabeledMorphism
  { domainGraph   :: LabeledGraph
  , codomainGraph :: LabeledGraph
  , nodeMap       :: EnumMap NodeId NodeId
  , edgeMap       :: EnumMap EdgeId EdgeId
  , variableMap   :: EnumMap VarId VarId
  }
  deriving Eq


instance Show LabeledMorphism where

  show m = concat
    [ "LNode mappings:\n"
    , concatMap showNodeMapping (EnumMap.toList $ nodeMap m)
    , "LEdge mappings:\n"
    , concatMap showEdgeMapping (EnumMap.toList $ edgeMap m)
    , "Variable renamings:\n"
    , concatMap showVarRenaming (EnumMap.toList $ variableMap m) ]
    where
      showNodeMapping (id1, id2) = "\t" ++ showNode id1 (domainGraph m) ++ " => " ++ showNode id2 (codomainGraph m) ++ "\n"
      showNode n graph = case lookupNode n graph of
        Nothing   -> "INVALID(" ++ show n ++ ")"
        Just node -> show n ++ " [" ++ label node ++ "]"
        where label = maybe "" showVar' . nodeLabel
      showEdgeMapping (id1, id2) = "\t" ++ showEdge id1 (domainGraph m) ++ " => " ++ showEdge id2 (codomainGraph m) ++ "\n"
      showEdge e graph = case lookupEdge e graph of
        Nothing -> "INVALID(" ++ show e ++ ")"
        Just edge -> show e ++ " [" ++ show (sourceId edge) ++ "->" ++ show (targetId edge) ++ "]"
      showVarRenaming (id1, id2) = showVar id1 domVars ++ " => " ++ showVar id2 codVars ++ "\n"
      showVar v varMap = case EnumMap.lookup v varMap of
        Nothing  -> "INVALID(" ++ show v ++ ")"
        Just var -> showVar' var
      showVar' (Variable v []) = show v
      showVar' (Variable v names) = show v ++ ":" ++ List.intercalate "," (map Text.unpack names)
      domVars = freeVariableMap (domainGraph m)
      codVars = freeVariableMap (codomainGraph m)


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
              (Just v, justW)   -> lookupVarId (varId v) morphism == (varId <$> justW)


-- * Query

isTotal, isTotalOnNodes, isTotalOnEdges, isTotalOnVariables :: LabeledMorphism -> Bool
isTotal m = isTotalOnNodes m && isTotalOnEdges m && isTotalOnVariables m

isTotalOnNodes m = EnumMap.keysSet (nodeMap m) == (EnumSet.fromList . nodeIds $ domainGraph m)
isTotalOnEdges m = EnumMap.keysSet (edgeMap m) == (EnumSet.fromList . edgeIds $ domainGraph m)
isTotalOnVariables m = EnumMap.keysSet (variableMap m) == freeVariableSet (domainGraph m)

isInjective :: LabeledMorphism -> Bool
isInjective m = isInjective' (nodeMap m) && isInjective' (edgeMap m) && isInjective' (variableMap m)
  where isInjective' = EnumMap.isInjective . EnumMap.toList

isSurjective :: LabeledMorphism -> Bool
isSurjective m =
  range (nodeMap m) == (EnumSet.fromList . nodeIds $ codomainGraph m)
    && range (edgeMap m) == (EnumSet.fromList . edgeIds $ codomainGraph m)
    && range (variableMap m) == freeVariableSet (codomainGraph m)
  where range = EnumSet.fromList . EnumMap.elems

isBijective :: LabeledMorphism -> Bool
isBijective m =
  isBijective' (nodeMap m) (EnumSet.fromList . nodeIds $ codomainGraph m)
    && isBijective' (edgeMap m) (EnumSet.fromList . edgeIds $ codomainGraph m)
    && isBijective' (variableMap m) (freeVariableSet $ codomainGraph m)
  where
    isBijective' m codomain = all isSingleton inverseM && EnumMap.keysSet inverseM == codomain
      where inverseM = EnumMap.inverse (EnumMap.toList m)
    isSingleton [_] = True
    isSingleton _   = False

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

lookupNodeId :: NodeId -> LabeledMorphism -> Maybe NodeId
lookupNodeId nodeId = EnumMap.lookup nodeId . nodeMap

lookupEdgeId :: EdgeId -> LabeledMorphism -> Maybe EdgeId
lookupEdgeId edgeId = EnumMap.lookup edgeId . edgeMap

lookupVarId :: VarId -> LabeledMorphism -> Maybe VarId
lookupVarId varId = EnumMap.lookup varId . variableMap

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
      -> [(NodeId, NodeId)] -> [(EdgeId, EdgeId)] -> [(VarId, VarId)]
      -> LabeledMorphism
fromGraphsAndLists dom cod nodeMap edgeMap varMap = LabeledMorphism
  { domainGraph = dom
  , codomainGraph = cod
  , nodeMap = EnumMap.fromList nodeMap
  , edgeMap = EnumMap.fromList edgeMap
  , variableMap = inducedRenaming `EnumMap.union` EnumMap.fromList varMap
  }
  where
    inducedRenaming = EnumMap.fromList
      [ (v1, v2)
          | (n1, n2) <- nodeMap
          , Just (Variable v1 _) <- [nodeLabel =<< lookupNode n1 dom]
          , Just (Variable v2 _) <- [nodeLabel =<< lookupNode n2 cod]
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
    composeMaps firstMap secondMap = EnumMap.fromList
        [ (x, z)
            | (x, y) <- EnumMap.toList firstMap
            , Just z <- [EnumMap.lookup y secondMap] ]
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

variableRelation :: LabeledMorphism -> Relation VarId
variableRelation f =
  Relation.fromPairs (freeVariableIdsOf $ domainGraph f) (freeVariableIdsOf $ codomainGraph f) (EnumMap.toList $ variableMap f)
