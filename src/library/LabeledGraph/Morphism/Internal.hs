{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE TypeFamilies              #-}
module LabeledGraph.Morphism.Internal
  (
  -- * Morphism type
    LabeledMorphism(..)

  -- * Query
  , isTotal
  , isTotalOnNodes
  , isTotalOnEdges
  , isTotalOnVariables
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
  , Abstract.Morphism.id
  , fromGraphsAndLists

  -- * Conversion
  -- ** Maps
  , nodeMap
  , edgeMap
  , variableMap

  -- ** Relations
  , nodeRelation
  , edgeRelation
  , variableRelation

  -- ** Lists
  , orphanNodes
  , orphanEdges
  , orphanVariables
  ) where


import           Abstract.Morphism
import           Abstract.Relation     (Relation)
import qualified Abstract.Relation     as Relation
import           Abstract.Valid
import           Abstract.Variable
import           Graph.Graph           hiding (edgeMap, nodeMap)
import           LabeledGraph.Internal

import           Control.Arrow
import           Data.Foldable         (foldl')
import           Data.IntMap           (IntMap)
import qualified Data.IntMap           as IntMap
import           Data.IntSet           (IntSet)
import qualified Data.IntSet           as IntSet
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (isJust, maybeToList)
import qualified Data.Set              as Set
import qualified Data.Text             as Text



data LabeledMorphism =
  LabeledMor
    { _domain          :: LabeledGraph
    , _codomain        :: LabeledGraph
    , nodeMapping      :: IntMap NodeId
    , edgeMapping      :: IntMap EdgeId
    , variableRenaming :: Map Variable Variable
    }
    deriving Eq


instance Show LabeledMorphism where

  show m =
      "LNode mappings:\n"
      ++ concatMap showNodeMapping (IntMap.toList $ nodeMapping m)
      ++ "LEdge mappings:\n"
      ++ concatMap showEdgeMapping (IntMap.toList $ edgeMapping m)
      ++ "Variable renamings:\n"
      ++ concatMap showVarRenaming (Map.toList $ variableRenaming m)

    where
      showNodeMapping (id1, id2) =
        let
          Just n1 = lookupNode (NodeId id1) (_domain m)
          Just n2 = lookupNode id2 (_codomain m)
        in
          "\t" ++ showNode n1 ++ " => " ++ showNode n2 ++ "\n"

      showNode n =
        let
          label =
            maybe "" Text.unpack . nodeLabel
        in
          show (nodeId n) ++ " [" ++ label n ++ "]"

      showEdgeMapping (id1, id2) =
        let
          Just e1 = lookupEdge (EdgeId id1) (_domain m)
          Just e2 = lookupEdge id2 (_codomain m)
        in
          "\t" ++ showEdge e1 ++ " => " ++ showEdge e2 ++ "\n"

      showEdge e =
        show (edgeId e) ++ " [" ++ show (sourceId e) ++ "->" ++ show (targetId e) ++ "]"

      showVarRenaming (v1, v2) =
        Text.unpack v1 ++ " => " ++ Text.unpack v2 ++ "\n"



instance Valid LabeledMorphism where

  validate morphism =
    mconcat
      [ withContext "domain" $ validate (domain morphism)
      , withContext "codomain" $ validate (codomain morphism)
      , ensure (isTotalOnNodes morphism) "the morphism is not total on nodes"
      , ensure (isTotalOnEdges morphism) "the morphism is not total on edges"
      , ensure (isTotalOnVariables morphism) "the morphism is not total on variables"
      , ensure preservesIncidence "the morphism doesn't preseve incidence/adjacency"
      , ensure preservesLabeling "the morphism doesn't preserve labeling"
      ]

    where
      preservesIncidence =
        let
          preservedAtEdge edge =
            (sourceId <$> applyToEdge edge morphism) == lookupNodeId (sourceId edge) morphism
            && (targetId <$> applyToEdge edge morphism) == lookupNodeId (targetId edge) morphism
        in
          all preservedAtEdge (edges $ domain morphism)



      preservesLabeling =
        let
          preservedAtNode node =
            case (nodeLabel node, nodeLabel =<< applyToNode node morphism) of
              (Nothing, _) ->
                True

              (Just _, Nothing) ->
                False

              (Just v, justW) ->
                applyToVariable v morphism == justW
        in
          all preservedAtNode (nodes $ domain morphism)



instance Morphism LabeledMorphism where

  type Obj LabeledMorphism =
    LabeledGraph


  {-# INLINE domain #-}
  domain =
    _domain


  {-# INLINE codomain #-}
  codomain =
    _codomain


  {-# INLINE id #-}
  id graph =
    LabeledMor
      { _domain = graph
      , _codomain = graph
      , nodeMapping = IntMap.fromList [ (fromEnum n, n) | n <- nodeIds graph ]
      , edgeMapping = IntMap.fromList [ (fromEnum e, e) | e <- edgeIds graph ]
      , variableRenaming = Map.fromList [ (v, v) | v <- freeVariablesOf graph ]
      }


  {-# INLINE compose #-}
  compose f g =
    LabeledMor
      { _domain = _domain f
      , _codomain = _codomain g
      , nodeMapping = composeIntMaps (nodeMapping f) (nodeMapping g)
      , edgeMapping = composeIntMaps (edgeMapping f) (edgeMapping g)
      , variableRenaming = composeMaps (variableRenaming f) (variableRenaming g)
      }

    where
      composeIntMaps firstMap secondMap =
        IntMap.fromList
          [ (x, z)
              | (x, y) <- IntMap.toList firstMap
              , z <- maybeToList $ IntMap.lookup (fromEnum y) secondMap
          ]

      composeMaps firstMap secondMap =
        Map.fromList
          [ (x, z)
              | (x, y) <- Map.toList firstMap
              , z <- maybeToList $ Map.lookup y secondMap
          ]

  isMonomorphism m =
      isInjective (IntMap.elems $ nodeMapping m) && isInjective (IntMap.elems $ nodeMapping m)
        && isInjective (Map.elems $ variableRenaming m)

    where
      isInjective pairs =
        let
          preimageCounts = foldl' incrementCount Map.empty pairs
        in
          any (>1) preimageCounts

      incrementCount counts x =
        Map.insertWith (+) x (1 :: Int) counts


  isEpimorphism m =
      intMapRange (nodeMapping m) == asIntSet (nodeIds $ _codomain m)
        && intMapRange (edgeMapping m) == asIntSet (edgeIds $ _codomain m)
        && mapRange (variableRenaming m) == freeVariableSet (_codomain m)

    where
      intMapRange =
        IntSet.fromList . map fromEnum . IntMap.elems

      mapRange =
        Set.fromList . Map.elems


  isIsomorphism m =
    isMonomorphism m && isEpimorphism m



-- * Query


isTotal :: LabeledMorphism -> Bool
isTotal m =
    isTotalOnNodes m && isTotalOnEdges m && isTotalOnVariables m


isTotalOnNodes :: LabeledMorphism -> Bool
isTotalOnNodes m =
  IntMap.keysSet (nodeMapping m) == asIntSet (nodeIds $ domain m)


isTotalOnEdges :: LabeledMorphism -> Bool
isTotalOnEdges m =
  IntMap.keysSet (edgeMapping m) == asIntSet (edgeIds $ domain m)


isTotalOnVariables :: LabeledMorphism -> Bool
isTotalOnVariables m =
  Map.keysSet (variableRenaming m) == freeVariableSet (domain m)


applyToNode :: LNode -> LabeledMorphism -> Maybe LNode
applyToNode =
  applyToNodeId . nodeId


applyToEdge :: LEdge -> LabeledMorphism -> Maybe LEdge
applyToEdge =
  applyToEdgeId . edgeId


applyToNodeId :: NodeId -> LabeledMorphism -> Maybe LNode
applyToNodeId nodeId f = do
  nodeId' <- lookupNodeId nodeId f
  lookupNode nodeId' (codomain f)


applyToEdgeId :: EdgeId -> LabeledMorphism -> Maybe LEdge
applyToEdgeId edgeId f = do
  edgeId' <- lookupEdgeId edgeId f
  lookupEdge edgeId' (codomain f)


applyToVariable :: Variable -> LabeledMorphism -> Maybe Variable
applyToVariable var =
  Map.lookup var . variableRenaming


lookupNodeId :: NodeId -> LabeledMorphism -> Maybe NodeId
lookupNodeId nodeId =
  IntMap.lookup (fromEnum nodeId) . nodeMapping


lookupEdgeId :: EdgeId -> LabeledMorphism -> Maybe EdgeId
lookupEdgeId edgeId =
  IntMap.lookup (fromEnum edgeId) . edgeMapping


isNodeDefined :: NodeId -> LabeledMorphism -> Bool
isNodeDefined nodeId m =
  isJust (applyToNodeId nodeId m)


isEdgeDefined :: EdgeId -> LabeledMorphism -> Bool
isEdgeDefined edgeId m =
  isJust (applyToEdgeId edgeId m)



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
--    undefined label, or the label of a node in the domain is not mapped to the label of the
--    corresponding node in the codomain)
fromGraphsAndLists
  :: LabeledGraph -> LabeledGraph
      -> [(NodeId, NodeId)] -> [(EdgeId, EdgeId)] -> [(Variable, Variable)]
      -> LabeledMorphism
fromGraphsAndLists dom cod nodeMap edgeMap varMap =
  LabeledMor
    { _domain = dom
    , _codomain = cod
    , nodeMapping = asIntMap nodeMap
    , edgeMapping = asIntMap edgeMap
    , variableRenaming = inducedRenaming `Map.union` Map.fromList varMap
    }
  where
    inducedRenaming =
      Map.fromList
        [ (v1, v2)
            | (n1, n2) <- nodeMap
            , v1 <- maybeToList $ nodeLabel =<< lookupNode n1 dom
            , v2 <- maybeToList $ nodeLabel =<< lookupNode n2 dom
        ]



-- * Conversion
-- ** Maps


nodeMap :: LabeledMorphism -> IntMap NodeId
nodeMap =
  nodeMapping


edgeMap :: LabeledMorphism -> IntMap EdgeId
edgeMap =
  edgeMapping


variableMap :: LabeledMorphism -> Map Variable Variable
variableMap =
  variableRenaming



-- ** Relations

nodeRelation :: LabeledMorphism -> Relation NodeId
nodeRelation f =
  Relation.fromPairs (nodeIds $ domain f) (nodeIds $ codomain f)
    $ map (first toEnum) (IntMap.toList $ nodeMapping f)


edgeRelation :: LabeledMorphism -> Relation EdgeId
edgeRelation f =
  Relation.fromPairs (edgeIds $ domain f) (edgeIds $ codomain f)
    $ map (first toEnum) (IntMap.toList $ edgeMapping f)


variableRelation :: LabeledMorphism -> Relation Variable
variableRelation f =
  Relation.fromPairs
    (freeVariablesOf $ domain f)
    (freeVariablesOf $ codomain f)
    (Map.toList $ variableRenaming f)



-- ** Lists

orphanNodes :: LabeledMorphism -> [NodeId]
orphanNodes f =
  let
    orphans =
      IntSet.difference
        (asIntSet . nodeIds $ domain f)
        (IntMap.keysSet $ nodeMapping f)
  in
    map toEnum (IntSet.toList orphans)


orphanEdges :: LabeledMorphism -> [EdgeId]
orphanEdges f =
  let
    orphans =
      IntSet.difference
        (asIntSet . edgeIds $ domain f)
        (IntMap.keysSet $ edgeMapping f)
  in
    map toEnum (IntSet.toList orphans)


orphanVariables :: LabeledMorphism -> [Variable]
orphanVariables f =
  Set.toList $
    Set.difference
      (freeVariableSet $ domain f)
      (Map.keysSet $ variableRenaming f)



-- * Utilities


asIntMap :: Enum k => [(k, v)] -> IntMap v
asIntMap =
  IntMap.fromList . map (first fromEnum)


asIntSet :: Enum a => [a] -> IntSet
asIntSet =
  IntSet.fromList . map fromEnum
