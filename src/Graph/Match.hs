{-# LANGUAGE TypeFamilies #-}
module Graph.Match
    ( findMatches
    , MorphismType (..)
    )
    where

import Control.Monad (foldM)
import Control.Monad.State
import qualified Data.List as L
import Data.List.Utils
import Data.Maybe -- (fromMaybe)
import Graph.Graph (Graph, edges, Edge, EdgeId, incidentEdges, Node, 
              NodeId, nodes, null, sourceOf, targetOf, removeNode, removeEdge)
import Graph.GraphRule (GraphRule)
import qualified Graph.GraphRule as GR
import qualified Data.Set as Set
import Abstract.Morphism (domain, codomain, epimorphism)
import Graph.GraphMorphism (GraphMorphism, TypedGraph)
import qualified Graph.GraphMorphism as GM
import qualified Abstract.Relation as R
import Graph.TypedGraphMorphism (mapping, typedMorphism, TypedGraphMorphism)

-- | 'MorphismType' forces 'findMatches' to only return the desired morphisms.
-- It's used to reduce the search space when possible.
data MorphismType = Normal | Mono | Epi | Iso 
    deriving (Eq)

-- | unwrap 'sourceOf' from the Maybe monad.
unsafeSourceOf :: Graph a b -> EdgeId -> NodeId
unsafeSourceOf g e =
    case sourceOf g e of
        Just s -> s
        Nothing -> error "Edge e doesn't exist, or has no source node"

-- | unwrap 'targetOf' from the Maybe monad.
unsafeTargetOf :: Graph a b -> EdgeId -> NodeId
unsafeTargetOf g e =
    case targetOf g e of
        Just s -> s
        Nothing -> error "Edge e doesn't exist, or has no target node"

-- | Return a list all possible homomorphisms between the graphs, filtering
-- out morphisms that would be invalid according to the given rule and morphism
-- type.
findMatches :: (Eq a, Eq b)
            => GraphRule a b
            -> MorphismType
            -> TypedGraph a b
            -> TypedGraph a b
            -> [TypedGraphMorphism a b]
findMatches rule mt l r = 
    map getMatch . matchGraphs $
        MatchState rule mt l r rg (edges lg) (nodes lg) $
                   typedMorphism l r $ GM.empty lg rg
  where
    lg = domain l
    rg = domain r
    

data MatchState a b = MatchState {
    getRule         :: GraphRule a b, -- ^ the (unchanged) rule that guides the matching.
    getMorphismType :: MorphismType,
    getLTypedGraph  :: TypedGraph a b,
    getRTypedGraph  :: TypedGraph a b,
    getRGraph       :: Graph a b, -- ^ modified to reduce matching algorithm's domain
    getEdges        :: [EdgeId], -- ^ edges to match
    getNodes        :: [NodeId], -- ^ nodes to match
    getMatch        :: TypedGraphMorphism a b
    }

-- | The main algorithm that return all possible matches between the two
-- graphs inside the given MatchState.
matchGraphs :: (Eq a, Eq b)
            => MatchState a b
            -> [MatchState a b]
matchGraphs st@(MatchState _ mt _ r rg [] [] m) =
    case mt of
    Epi | rg == domain r -> [st]
        | otherwise     -> []
    Iso | GM.null r  -> [st]
        | otherwise     -> []
    otherwise -> [st]
    
matchGraphs st@(MatchState rule mt l r rg (le:les) lns m) =
    matchAllEdges le candidates >>= matchGraphs
  where
    lg = domain l
    candidates = filter (hasLoop st le) $
                 filter (matchesSameSource st le) $
                 filter (matchesSameTarget st le) $
                 querySameTypeEdges st le
    leSrc = unsafeSourceOf lg le
    leTgt = unsafeTargetOf lg le
    reSrc re = unsafeSourceOf rg re
    reTgt re = unsafeTargetOf rg re
    lns' = L.delete leSrc $ L.delete leTgt lns
    rg' re | mt == Normal || mt == Epi = rg
           | otherwise = removeNode (reSrc re) $
                         removeNode (reTgt re) $
                         removeEdge re rg
    matchEdges le re =
        MatchState rule mt l r (rg' re) les lns' $
           typedMorphism (domain m) (codomain m) $
               GM.updateEdges le re $
               GM.updateNodes (unsafeSourceOf lg le) (unsafeSourceOf rg re) $
               GM.updateNodes (unsafeTargetOf lg le) (unsafeTargetOf rg re) $
               mapping m
    matchAllEdges le res =
        map (matchEdges le) res
    morphism = mapping m

matchGraphs st@(MatchState rule mt l r rg [] (ln:lns) m) =
    matchAllNodes ln candidates' >>= matchGraphs
  where
    matchAllNodes ln rns =
        map (matchNodes ln) rns
    matchNodes ln rn =
        MatchState rule mt l r (rg' rn) [] lns $
            typedMorphism (domain m) (codomain m) $
                GM.updateNodes ln rn morphism
    candidates = filter noIdentityConflict $ querySameTypeNodes st ln
    candidates' | toBeDeleted ln = filter noDangling candidates
                | otherwise = candidates
    toBeDeleted ln = ln `L.elem` deletedFromL
    toBeDeleted' rn = rn `L.elem` deletedFromR 
    deletedFromL = deletedNodes rule
    deletedFromR = deletedFromL >>= GM.applyNode morphism
    noDangling = L.null . incidentEdges rg
    noIdentityConflict rn = notMapped rn || (toBeDeleted ln && toBeDeleted' rn)
    notMapped rn = rn `L.notElem` R.image rel
    morphism = mapping m
    rel = GM.nodeRelation morphism
    rg' rn | mt == Normal || mt == Epi = rg
           | otherwise = removeNode rn rg
    
-- | Return a list of all nodes in the right-side graph that has the same
-- type as @n@.
querySameTypeNodes :: MatchState a b -> NodeId -> [NodeId]
querySameTypeNodes st n =
    GM.applyNode rinv typeId `L.intersect` remainingNodes
  where
    l = getLTypedGraph st
    r = getRTypedGraph st
    rg = getRGraph st
    remainingNodes = nodes rg
    rinv = GM.inverse r
    [typeId] = GM.applyNode l n

-- | Return a list of all edges in the right-side graph that has the same
-- type as @n@.
querySameTypeEdges :: MatchState a b -> EdgeId -> [EdgeId]
querySameTypeEdges st eid =
    GM.applyEdge rinv typeId `L.intersect` remainingEdges
  where
    l = getLTypedGraph st
    r = getRTypedGraph st
    rg = getRGraph st
    remainingEdges = edges rg
    rinv = GM.inverse r
    [typeId] = GM.applyEdge l eid

-- | In case @le@'s source already got mapped, check if @re@'s source is the
-- same mapped node.
matchesSameSource :: (Eq a, Eq b) => MatchState a b -> EdgeId -> EdgeId -> Bool
matchesSameSource st le re =
    let rnodes = GM.applyNode m leSrc
    in case rnodes of
        (x:xs)    -> x == reSrc
        otherwise -> True
  where
    l = domain $ getLTypedGraph st
    r = domain $ getRTypedGraph st
    m = mapping $ getMatch st
    leSrc = unsafeSourceOf l le
    reSrc = unsafeSourceOf r re

-- | In case @le@'s target already got mapped, check if @re@'s target is the
-- same mapped node.
matchesSameTarget :: (Eq a, Eq b) => MatchState a b -> EdgeId -> EdgeId -> Bool
matchesSameTarget st le re =
    let rnodes = GM.applyNode m leTgt
    in case rnodes of
        (x:xs)    -> x == reTgt
        otherwise -> True
  where
    l = domain $ getLTypedGraph st
    r = domain $ getRTypedGraph st
    m = mapping $ getMatch st
    leTgt = unsafeTargetOf l le
    reTgt = unsafeTargetOf r re

-- | If @le@ is a loop edge, forces that @re@ also forms a loop.
hasLoop :: (Eq a, Eq b) => MatchState a b -> EdgeId -> EdgeId -> Bool
hasLoop st le re
    | leSrc == leTgt = reSrc == reTgt
    | otherwise = True
  where
    l = domain $ getLTypedGraph st
    r = domain $ getRTypedGraph st
    leSrc = unsafeSourceOf l le
    reSrc = unsafeSourceOf r re
    leTgt = unsafeTargetOf l le
    reTgt = unsafeTargetOf r re

-- | Return a list of all nodes removed by @rule@.
deletedNodes :: (Eq a, Eq b) => GraphRule a b -> [NodeId]
deletedNodes rule =
    (L.\\) defDom img
  where
    lSide = GR.left rule
    morphism = mapping lSide
    nodeMapping = GM.nodeRelation morphism
    defDom = R.defDomain nodeMapping
    img    = R.image nodeMapping

