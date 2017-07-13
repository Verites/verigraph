{-# LANGUAGE OverloadedStrings #-}
module Category.LabeledGraph.JointlyEpimorphisms where

import           Control.Applicative
import           Control.Monad
import           Data.Function                          (on)
import qualified Data.List                              as List
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Maybe                             (isNothing, mapMaybe)
import qualified Data.Set                               as Set

import           Abstract.Category.JointlyEpimorphisms
import           Category.LabeledGraph.FinitaryCategory ()
import           Data.EnumMap                           (EnumMap)
import qualified Data.EnumMap                           as EnumMap
import           Data.LabeledGraph
import           Data.LabeledGraph.Morphism
import           Data.Partition
import           Data.Variable
import qualified Util.Map                               as Map


instance JointlyEpimorphisms LabeledMorphism where

  createAllQuotients g = do
    variablesPartition <- allPartitionsOf (freeVariableIdsOf g)
    let varMap = partitionToSurjection variablesPartition $ \identifiedIds ->
          let
            representativeId = Set.findMin identifiedIds
            names = concat . mapMaybe (fmap varNameHints . lookupVarOnG) $ Set.toList identifiedIds
          in Variable representativeId names
    let varIdMap = fmap varId varMap

    -- Two labeled nodes can only be identified if they have the same label. Thus, we first
    -- partition labeled nodes according to their label, then pick a refinement of this partition,
    -- then add unlabeled nodes to it (identifing them with any other node).
    let renamedNodes =
          [Node n (Map.lookupMaybe (varId <$> v) varMap) | Node n v <- nodes g]
    let (unlabeledNodes, labeledNodes) = List.partition (isNothing . nodeLabel) renamedNodes
    let nodeLabels = EnumMap.fromList [(nodeId n, v) | n <- labeledNodes, let Just v = nodeLabel n]

    let groupedNodesByLabel = [map nodeId group | group <- partitionBy (fmap varId . nodeLabel) labeledNodes]
    labeledNodesPartition <- allRefinementsOf (toPartition groupedNodesByLabel)
    nodesPartition <- foldM (flip addToPartition) labeledNodesPartition (map nodeId unlabeledNodes)
    let nodeMap = partitionToSurjection nodesPartition $ \identifiedIds ->
          let
            representativeId = Set.findMin identifiedIds
            label = maybeHead $ mapMaybe (`EnumMap.lookup` nodeLabels) (Set.toList identifiedIds)
          in Node representativeId label
    let nodeIdMap = fmap nodeId nodeMap

    -- Two edges can only be identified if they have the same source/target pair. Thus, we first
    -- partition edges according to theirs source/target pairs, then pick a refinement.
    let renamedEdges =
          [ Edge e (nodeIdMap Map.! src) (nodeIdMap Map.! tgt) () | Edge e src tgt () <- edges g ]
    let groupedEdgesBySrcTgt = [map edgeId group | group <- partitionBy sourceTarget renamedEdges]
    edgesPartition <- allRefinementsOf (toPartition groupedEdgesBySrcTgt)
    let edgeIdMap = partitionToSurjection edgesPartition Set.findMin

    let quotientGraph = fromNodesAndEdges (Map.elems nodeMap) (filterRepresentatives edgeId edgeIdMap renamedEdges)
    return $ LabeledMorphism g quotientGraph
      (EnumMap.fromList . Map.toList $ nodeIdMap)
      (EnumMap.fromList . Map.toList $ edgeIdMap)
      (EnumMap.fromList . Map.toList $ varIdMap)
    where
      sourceTarget (Edge _ src tgt _) = (src, tgt)
      lookupVarOnG v = EnumMap.lookup v gVarMap
      gVarMap = freeVariableMap g

  createMonicJointlyEpimorphicPairs gL gR = do
    varIdentifications <- pickIdentifications (freeVariablesOf gL) (freeVariablesOf gR) $ \_ _ -> True
    let (varMapL, varMapR, _) = identificationsToMaps varIdentifications varId
          (\(Variable _ hints) v -> Variable v hints)
          (\(Variable _ hints1) (Variable _ hints2) v -> Variable v (hints1 ++ hints2))

    let nodesL = renameVariables varMapL (nodes gL)
    let nodesR = renameVariables varMapR (nodes gR)
    nodeIdentifications <- pickIdentifications nodesL nodesR $ \nL nR ->
                              case (nodeLabel nL, nodeLabel nR) of
                                (Nothing, _)       -> True
                                (_, Nothing)       -> True
                                (Just vL, Just vR) -> varId vL == varId vR
    let (nodeMapL, nodeMapR, codNodes) = identificationsToMaps nodeIdentifications nodeId
          (\(Node _ v) n -> Node n v)
          (\(Node _ vl) (Node _ vr) n -> Node n (vl <|> vr))

    let edgesL = map (renameSourceTarget nodeMapL) (edges gL)
    let edgesR = map (renameSourceTarget nodeMapR) (edges gR)
    edgeIdentifications <- pickIdentifications edgesL edgesR $ \eL eR ->
                              sourceId eL == sourceId eR && targetId eL == targetId eR
    let (edgeMapL, edgeMapR, codEdges) = identificationsToMaps edgeIdentifications edgeId
          (\(Edge _ src tgt _) e -> Edge e src tgt ())
          (\(Edge _ src tgt _) _ e -> Edge e src tgt ())

    let codomainGraph = fromNodesAndEdges codNodes codEdges
    return
      ( LabeledMorphism gL codomainGraph (EnumMap.map nodeId nodeMapL) (EnumMap.map edgeId edgeMapL) (EnumMap.map varId varMapL)
      , LabeledMorphism gR codomainGraph (EnumMap.map nodeId nodeMapR) (EnumMap.map edgeId edgeMapR) (EnumMap.map varId varMapR)
      )
    where
      renameSourceTarget nodeMap (Edge e src tgt ()) =
        Edge e (nodeId $ nodeMap EnumMap.! src) (nodeId $ nodeMap EnumMap.! tgt) ()


type Identifications a = ([a], [(a, a)], [a])

pickIdentifications :: [a] -> [a] -> (a -> a -> Bool) -> [Identifications a]
pickIdentifications xs ys isIdentifiable = recurse ([], [], []) xs ys
  where
    recurse (disjointX, identified, disjointY) [] ys = [(disjointX, identified, ys ++ disjointY)]
    recurse (disjointX, identified, disjointY) xs [] = [(xs ++ disjointX, identified, disjointY)]
    recurse (disjointX, identified, disjointY) (x:xs) ys = concat $
      recurse (x:disjointX, identified, disjointY) xs ys :
      [ recurse (disjointX, (x,y):identified, disjointY) xs ys'
          | (y,ys') <- allSplits ys, isIdentifiable x y ]

identificationsToMaps :: Enum k => Identifications a -> (a -> k) -> (a -> k -> v) -> (a -> a -> k -> v) -> (EnumMap k v, EnumMap k v, [v])
identificationsToMaps (elemsL, joinedElements, elemsR) idOf makeSingle makeJoined =
  let
    pairsL = [ (idOf l, makeSingle l v) | (l, v) <- zip elemsL [toEnum 0..] ]
    pairsR = [ (idOf r, makeSingle r v) | (r, v) <- zip elemsR [toEnum (length elemsL)..] ]
    pairsJoined = [ (idOf l, idOf r, makeJoined l r v)
                    | ((l, r), v) <- zip joinedElements [toEnum (length elemsL + length elemsR)..] ]
  in
    ( EnumMap.fromList $ pairsL ++ [ (l, e) | (l, _, e) <- pairsJoined ]
    , EnumMap.fromList $ pairsR ++ [ (r, e) | (_, r, e) <- pairsJoined ]
    , map snd pairsL ++ map snd pairsR ++ map (\(_,_,e) -> e) pairsJoined
    )

allSplits :: [a] -> [(a, [a])]
allSplits [] = []
allSplits [x] = [(x, [])]
allSplits (x:xs) =
  (x, xs) :
  [ (x', x:xs') | (x', xs') <- allSplits xs ]



type ListPartition a = [[a]]

toPartition :: Ord a => ListPartition a -> Partition a
toPartition = Set.fromList . map Set.fromList

-- | Filter a list of ids, keeping only the elements that are in the image of the given map.
filterRepresentatives :: Ord a => (t -> a) -> Map k a -> [t] -> [t]
filterRepresentatives getId idMap =
  let representativeIds = Set.fromList (Map.elems idMap)
  in filter (\e -> Set.member (getId e) representativeIds)

-- | Given a projection function, partition a list such that each group has elements with equal
-- projections.
partitionBy :: Ord b => (a -> b) -> [a] -> ListPartition a
partitionBy projection = List.groupBy ((==) `on` projection) . List.sortBy (compare `on` projection)

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x
