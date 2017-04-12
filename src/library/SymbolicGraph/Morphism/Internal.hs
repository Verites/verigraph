{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE TypeFamilies              #-}
module SymbolicGraph.Morphism.Internal
  (
  -- * Morphism type
    SymbolicMorphism(..)

  -- * Query
  , isTotal
  , applyToNode
  , applyToEdge
  , applyToVariable
  , applyToNodeId
  , applyToEdgeId
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
import           Abstract.Relation         (Relation)
import qualified Abstract.Relation         as Relation
import           Abstract.Valid
import           SymbolicGraph.DataAlgebra
import           SymbolicGraph.Internal

import           Control.Arrow
import           Data.Foldable             (foldl')
import           Data.IntMap               (IntMap)
import qualified Data.IntMap               as IntMap
import           Data.IntSet               (IntSet)
import qualified Data.IntSet               as IntSet
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (isJust, maybeToList)
import qualified Data.Set                  as Set
import qualified Data.Text                 as Text



data SymbolicMorphism =
  SymbMor
    { _domain          :: SymbolicGraph
    , _codomain        :: SymbolicGraph
    , nodeMapping      :: IntMap NodeId
    , edgeMapping      :: IntMap EdgeId
    , variableRenaming :: Map Variable Variable
    }
    deriving Eq


instance Show SymbolicMorphism where

  show m =
      "Node mappings:\n"
      ++ concatMap showNodeMapping (IntMap.toList $ nodeMapping m)
      ++ "Edge mappings:\n"
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
          attr =
            maybe "" Text.unpack . nodeAttribute
        in
          show (nodeId n) ++ " [" ++ attr n ++ "]"

      showEdgeMapping (id1, id2) =
        let
          Just e1 = lookupEdge (EdgeId id1) (_domain m)
          Just e2 = lookupEdge id2 (_codomain m)
        in
          "\t" ++ showEdge e1 ++ " => " ++ showEdge e2 ++ "\n"

      showEdge e =
        show (edgeId e) ++ " [" ++ show (sourceId e) ++ "->" ++ show (targetId e) ++ "]"

      showVarRenaming (v1, v2) =
        Text.unpack v1 ++ " => " ++ Text.unpack v2



instance Valid SymbolicMorphism where

  validate morphism =
    mconcat
      [ withContext "domain" $ validate (domain morphism)
      , withContext "codomain" $ validate (codomain morphism)
      , ensure (isTotal morphism) "the morphism is not total"
      , ensure preservesIncidence "the morphism doesn't preseve incidence/adjacency"
      , ensure preservesAttribution "the morphism doesn't preserve attribution"
      -- TODO check strengthening
      ]

    where
      preservesIncidence =
        let
          preservedAtEdge edge =
            (sourceId <$> applyToEdge edge morphism) == applyToNodeId (sourceId edge) morphism
            && (targetId <$> applyToEdge edge morphism) == applyToNodeId (targetId edge) morphism
        in
          all preservedAtEdge (edges $ domain morphism)



      preservesAttribution =
        let
          preservedAtNode node =
            case (nodeAttribute node, nodeAttribute =<< applyToNode node morphism) of
              (Nothing, _) ->
                True

              (Just _, Nothing) ->
                False

              (justV, Just w) ->
                justV == applyToVariable w morphism
        in
          all preservedAtNode (nodes $ domain morphism)



instance Morphism SymbolicMorphism where

  type Obj SymbolicMorphism =
    SymbolicGraph


  {-# INLINE domain #-}
  domain =
    _domain


  {-# INLINE codomain #-}
  codomain =
    _codomain


  {-# INLINE id #-}
  id graph =
    SymbMor
      { _domain = graph
      , _codomain = graph
      , nodeMapping = IntMap.fromList [ (fromEnum n, n) | n <- nodeIds graph ]
      , edgeMapping = IntMap.fromList [ (fromEnum e, e) | e <- edgeIds graph ]
      , variableRenaming = Map.fromList [ (v, v) | v <- Set.toList (freeVariablesOf graph) ]
      }


  {-# INLINE compose #-}
  compose f g =
    SymbMor
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
        && mapRange (variableRenaming m) == freeVariablesOf (_codomain m)

    where
      intMapRange =
        IntSet.fromList . map fromEnum . IntMap.elems

      mapRange =
        Set.fromList . Map.elems


  isIsomorphism m =
    isMonomorphism m && isEpimorphism m



-- * Query


isTotal :: SymbolicMorphism -> Bool
isTotal m =
  IntMap.keysSet (nodeMapping m) == asIntSet (nodeIds $ domain m)
    && IntMap.keysSet (edgeMapping m) == asIntSet (nodeIds $ domain m)
    && Map.keysSet (variableRenaming m) == freeVariablesOf (domain m)


applyToNode :: Node -> SymbolicMorphism -> Maybe Node
applyToNode node f = do
  nodeId' <- applyToNodeId (nodeId node) f
  lookupNode nodeId' (codomain f)


applyToEdge :: Edge -> SymbolicMorphism -> Maybe Edge
applyToEdge edge f = do
  edgeId' <- applyToEdgeId (edgeId edge) f
  lookupEdge edgeId' (codomain f)


applyToVariable :: Variable -> SymbolicMorphism -> Maybe Variable
applyToVariable var =
  Map.lookup var . variableRenaming


applyToNodeId :: NodeId -> SymbolicMorphism -> Maybe NodeId
applyToNodeId nodeId =
  IntMap.lookup (fromEnum nodeId) . nodeMapping


applyToEdgeId :: EdgeId -> SymbolicMorphism -> Maybe EdgeId
applyToEdgeId edgeId =
  IntMap.lookup (fromEnum edgeId) . edgeMapping


isNodeDefined :: NodeId -> SymbolicMorphism -> Bool
isNodeDefined nodeId m =
  isJust (applyToNodeId nodeId m)


isEdgeDefined :: EdgeId -> SymbolicMorphism -> Bool
isEdgeDefined edgeId m =
  isJust (applyToEdgeId edgeId m)



-- * Construction


-- | Creates a morphism between the given symbolic graphs, mapping nodes and edges according to the
-- given lists. The variable renaming is constructed by first ensuring consistency with the node
-- mapping and attribution, then mapping additional variables according to the given list.
--
-- WARNING: the result could be an invalid morphism for any of the following reasons:
--  * Some node id, edge id or variable isn't part of the appropriate graph
--  * Incidence is not preserved (i.e. the source/target of an edge is mapped to a node that is not
--    the source/target of the mapped edge)
--  * Attribution is not preserved (i.e. a node with defined attribute is mapped to a node with
--    undefined attribute)
--  * Restrictions are not stregthened (i.e. the restrictions of the domain don't necessarily hold
--    on the codomain)
fromGraphsAndLists
  :: SymbolicGraph -> SymbolicGraph
      -> [(NodeId, NodeId)] -> [(EdgeId, EdgeId)] -> [(Variable, Variable)]
      -> SymbolicMorphism
fromGraphsAndLists dom cod nodeMap edgeMap varMap =
  SymbMor
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
            , v1 <- maybeToList $ nodeAttribute =<< lookupNode n1 dom
            , v2 <- maybeToList $ nodeAttribute =<< lookupNode n2 dom
        ]



-- * Conversion
-- ** Maps


nodeMap :: SymbolicMorphism -> IntMap NodeId
nodeMap =
  nodeMapping


edgeMap :: SymbolicMorphism -> IntMap EdgeId
edgeMap =
  edgeMapping


variableMap :: SymbolicMorphism -> Map Variable Variable
variableMap =
  variableRenaming



-- ** Relations

nodeRelation :: SymbolicMorphism -> Relation NodeId
nodeRelation f =
  Relation.fromPairs (nodeIds $ domain f) (nodeIds $ codomain f)
    $ map (first toEnum) (IntMap.toList $ nodeMapping f)


edgeRelation :: SymbolicMorphism -> Relation EdgeId
edgeRelation f =
  Relation.fromPairs (edgeIds $ domain f) (edgeIds $ codomain f)
    $ map (first toEnum) (IntMap.toList $ edgeMapping f)


variableRelation :: SymbolicMorphism -> Relation Variable
variableRelation f =
  Relation.fromPairs
    (Set.toList . freeVariablesOf $ domain f)
    (Set.toList . freeVariablesOf $ codomain f)
    (Map.toList $ variableRenaming f)



-- ** Lists

orphanNodes :: SymbolicMorphism -> [NodeId]
orphanNodes f =
  let
    orphans =
      IntSet.difference
        (asIntSet . nodeIds $ domain f)
        (IntMap.keysSet $ nodeMapping f)
  in
    map toEnum (IntSet.toList orphans)


orphanEdges :: SymbolicMorphism -> [EdgeId]
orphanEdges f =
  let
    orphans =
      IntSet.difference
        (asIntSet . edgeIds $ domain f)
        (IntMap.keysSet $ edgeMapping f)
  in
    map toEnum (IntSet.toList orphans)


orphanVariables :: SymbolicMorphism -> [Variable]
orphanVariables f =
  Set.toList $
    Set.difference
      (freeVariablesOf $ domain f)
      (Map.keysSet $ variableRenaming f)



-- * Utilities


asIntMap :: Enum k => [(k, v)] -> IntMap v
asIntMap =
  IntMap.fromList . map (first fromEnum)


asIntSet :: Enum a => [a] -> IntSet
asIntSet =
  IntSet.fromList . map fromEnum
