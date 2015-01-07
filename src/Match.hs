{-# LANGUAGE TypeFamilies #-}
module Match
    ( findMatches
    , MorphismType (..)
    )
    where

import Control.Monad (foldM)
import Control.Monad.State
import qualified Data.List as L
import Data.List.Utils
import Data.Maybe
import Graph (Graph, edges, Edge, EdgeId, incidentEdges, Node, 
              NodeId, nodes, null, sourceOf, targetOf, removeNode, removeEdge)
import GraphRule (GraphRule)
import qualified GraphRule as GR
import qualified Data.Set as Set
import Morphism (domain, codomain, epimorphism)
import GraphMorphism (GraphMorphism, TypedGraph)
import qualified GraphMorphism as GM
import qualified Relation as R
import TypedGraphMorphism (mapping, typedMorphism, TypedGraphMorphism)

-- | Is a tuple of two relations regarding two graphs (possibly equal):
-- the first among their respective G.nodes, the other among their G.edges. Each
-- relation is described as a list of (Int, Int) tuples.

data MorphismType = Normal | Mono | Epi | Iso 
    deriving (Eq)

unsafeSourceOf :: Graph a b -> EdgeId -> NodeId
unsafeSourceOf g = head . sourceOf g

unsafeTargetOf :: Graph a b -> EdgeId -> NodeId
unsafeTargetOf g = head . targetOf g


-- | Given two typed graphs, return a list of typed graph morphisms, each 
-- representing a possible homomorphism between the graphs.
{-
findMatches :: MorphismType
            -> TypedGraph a b -- left graph
            -> TypedGraph a b -- right graph
            -> [TypedGraphMorphism a b]
findMatches = undefined
-}
--findMatches mt l g = 
--    findMatchesR (Morph.empty l g) mt l g

-- | Given two typed graphs and a rule, return a list of mappings, each
-- representing a possible homomorphism between the graphs. The matches
-- generated respect the given rule according to the DPO approach.

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
    
--findMatchesR rule mt l r = matchGraphs r mt l g


data MatchState a b = MatchState {
    getRule         :: GraphRule a b,
    getMorphismType :: MorphismType,
    getLTypedGraph  :: TypedGraph a b,
    getRTypedGraph  :: TypedGraph a b,
    getRGraph       :: Graph a b, -- modified to reduce matching algorithm's domain
    getEdges        :: [EdgeId], -- edges to match
    getNodes        :: [NodeId], -- nodes to match
    getMatch        :: TypedGraphMorphism a b
    }

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
    
querySameTypeNodes :: MatchState a b -> NodeId -> [NodeId]
querySameTypeNodes st nid =
    GM.applyNode rinv typeId `L.intersect` remainingNodes
  where
    l = getLTypedGraph st
    r = getRTypedGraph st
    rg = getRGraph st
    remainingNodes = nodes rg
    rinv = GM.inverse r
    [typeId] = GM.applyNode l nid

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

deletedNodes :: (Eq a, Eq b) => GraphRule a b -> [NodeId]
deletedNodes rule =
    (L.\\) defDom img
  where
    lSide = GR.left rule
    morphism = mapping lSide
    nodeMapping = GM.nodeRelation morphism
    defDom = R.defDomain nodeMapping
    img    = R.image nodeMapping

{-
    
mapGraphs r mt l (m@(nmatch, ematch), g, [], (ln:lns)) =
    let conds = (generateConds r l ln g m)
        candidates = filter (processNode conds) $ G.nodes g
        newMapSets = fmap processNodeCandidate candidates
    in newMapSets >>= mapGraphs r mt l
      where
        processNodeCandidate gid =
            ((addToAL nmatch ln gid, ematch),
             if mt == Normal || mt == Epi
                then g
                else G.removeNode gid g,
             [],
             lns)


-- | List all isomorphisms (represented as mappings) between both graphs.
findIsoMorphisms :: TypedGraph a b -> TypedGraph a b -> [Morphism a b]
findIsoMorphisms l g
    | numNodes l /= numNodes g ||
      numEdges l /= numEdges g = []
    | otherwise = findMatchesR (Morph.empty l g) Iso l g

{-
        else filter isInjective $
                         filter (isSurjective g) $
                                 findMatches Iso l g
-}

-- | Check if there's an isomorphism between two graphs.
isIsomorphic :: TypedGraph a b -> TypedGraph a b -> Bool
isIsomorphic a b = findIsoMorphisms a b /= []

-}
