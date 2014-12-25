module SimpleMatch
    (
    Mapping
    , findMatches
    , isSurjective
    --, isInjective
    , findIsoMorphisms
    , isIsomorphic
    , MorphismType (..)
    )
    where

import Control.Monad (foldM)
import Data.List
import Data.List.Utils
import Data.Maybe
import qualified GraphClass as G
import Graph (Graph)
import qualified MorphismClass as Morph
import Morphism (Morphism)
import qualified Data.Set as Set

-- | Is a tuple of two relations regarding two graphs (possibly equal):
-- the first among their respective G.nodes, the other among their G.edges. Each
-- relation is described as a list of (Int, Int) tuples.
type Mapping = ([(Int, Int)], [(Int, Int)])

data MorphismType = Normal | Mono | Epi | Iso 
    deriving (Eq)

-- | Given two typed graphs, return a list of mappings, each representing a
-- possible homomorphism between the graphs.
findMatches :: MorphismType -> Graph a b -> Graph a b -> [Mapping]
findMatches mt lg rg = matchGraphs mt lg rg

----------------------------------------------------------------------------
-- Edge related condition functions

-- An EdgeCondition checks if a node satisfies it's internal requirements.
type EdgeCondition b = Int -> Bool

-- | Generate a condition that tests if @le@'s source already occurs in @m@.
-- If that's the case, check if @ge@'s source is the same node to which @le@'s
-- source got mapped.  If so, @ge@ is a matching Edge. If @le@'s source doesn't
-- occur in @m@, any @ge@ will satisfy this condition.
srcIdCondGen :: Graph a b -> Int -> Mapping -> Graph a b -> EdgeCondition b
srcIdCondGen lg le m@(nmatches, _) rg =
    (\re ->
        let lsrc    = G.sourceOf le lg
            rsrc    = G.sourceOf re rg
            matched = find (\(s, t) -> Just s == lsrc) nmatches
        in case matched of        
            Just (_, n) -> rsrc == Just n
            otherwise -> True)

-- | Generate an edge condition that tests if @le@'s target already occurs in
-- @m@. If that's the case, check if @ge@'s target is the same node to which
-- @le@'s target got mapped.  If so, @ge@ is a matching Edge. If @le@'s target
-- doesn't occur in @m@, any @ge@ will satisfy this condition.
tarIdCondGen :: Graph a b -> Int -> Mapping -> Graph a b -> EdgeCondition b
tarIdCondGen lg le m@(nmatches, _) rg =
    (\re -> let ltar    = G.targetOf le lg
                rtar    = G.targetOf re rg
                matched = find (\(s, t) -> Just s == ltar) nmatches
            in case matched of        
                Just (_, n) -> rtar == Just n
                otherwise -> True)

-- | Generate an edge condition that checks If @le@ is a loop edge, check if
-- @ge@ is also a loop in @g@.

-- This condition is due to the sequential nature of processEdges. Conditions 
-- that check node coincidence (like @srcIdCond@) rely on previously mappings,
-- so they aren't able to detect a mapping node in the current step. 
-- Without @loopCond@, a loop edge that, e.g., happens to be the first to be
-- mapped passes srcIdCond and tarIdCond.
loopCondGen :: Graph a b -> Int -> Graph a b -> EdgeCondition b
loopCondGen lg le rg =
    (\re -> let lsrc = G.sourceOf le lg
                ltar = G.targetOf le lg
                rsrc = G.sourceOf re rg
                rtar = G.targetOf re rg
            in if lsrc == ltar
                then rsrc == rtar
                else True)

-- | Feed each generator it's parameters and collect the resulting
-- nodeconditions in a list.
generateEdgeConds
        :: Graph a b
        -> Int
        -> Graph a b
        -> Mapping
        -> [EdgeCondition b]
generateEdgeConds lg le rg m =
    srcIdCondGen lg le m rg :
    tarIdCondGen lg le m rg :
    loopCondGen lg le rg  :
    []


-- | If all conditions in @cl@ get satisfied by the given edge @ge@, return the
-- @m@ mapping with the new edge/G.nodes added. 
processEdge :: [EdgeCondition b] -> Graph a b -> Int -> Bool
processEdge cl g e = foldr (\c acc -> (c e) && acc) True cl 

-------------------------------------------------------------------------
-- Node related condition functions

-- A NodeCondition checks if a node satisfies it's internal requirements.
type NodeCondition a = Int -> Bool
                
-- | Apply each condition from @nodecl@ to the node @n@. Return True if all
-- conditions got satisfied.
processNode :: [NodeCondition a] -> Int -> Bool
processNode cl n = foldr (\c acc -> (c n) && acc) True cl
        
                
----------------------------------------------------------------------------
-- The matching algorithm

-- | Given a list of G.edges from @l@ to be matched and a specific mapping @m@, 
-- return a list of all possible mappings between these G.edges and those
-- from graph @g@, taking @m@ as initial mapping.
mapGraphs ::
    MorphismType
    -> Graph a b        -- ^ @l@, the "left side" graph
    -> (Mapping, Graph a b, [Int], [Int]) -- ^ @m@, what already got mapped
    -> [(Mapping, Graph a b, [Int], [Int])]
-- When everything got mapped.
mapGraphs mt _ ml@((nmap, emap), rg, [], []) =
    case mt of
    Epi ->
        let rMappedNodes = foldr (\(ln, rn) acc -> addToAL acc ln rn) [] nmap
            rMappedEdges = foldr (\(le, re) acc -> addToAL acc le re) [] emap
        in  if length rMappedNodes == numNodes rg && length rMappedEdges == numEdges rg
               then [ml]
               else []
    Iso -> if nullG rg
              then [ml]
              else []
    otherwise -> [ml]
mapGraphs mt lg (m@(nmap, emap), rg, (le:les), lns) =
    let conds = generateEdgeConds lg le rg m
        edgeList = G.edges rg
        candidates = filter (processEdge conds rg) $ edgeList
        newMappings = fmap (processEdgeCandidate rg) candidates
    in newMappings >>= mapGraphs mt lg
      where
        -- adds edge candidate to a mapping
        processEdgeCandidate rg re =
            let res = do
                rs <- G.sourceOf re rg
                rt <- G.targetOf re rg
                ls <- G.sourceOf le lg
                lt <- G.targetOf le lg
                let newLNodeList = filter (\nid -> (Just nid /= G.sourceOf le lg) && (Just nid /= G.targetOf le lg)) lns
                return (
                    ((\nm -> addToAL nm ls rs) $
                     (\nm -> addToAL nm lt rt) $
                     nmap,
                     addToAL emap le re),
                    if mt == Normal || mt == Epi
                       then rg
                       else G.removeNode rs $ G.removeNode rt $ G.removeEdge re rg,
                    les,
                    newLNodeList)
            in fromJust res
mapGraphs mt rl (m@(nmatch, ematch), rg, [], (ln:lns)) =
    let candidates = G.nodes rg
        newMapSets = fmap processNodeCandidate candidates
    in newMapSets >>= mapGraphs mt rl
      where
        processNodeCandidate gid =
            ((addToAL nmatch ln gid, ematch),
             if mt == Normal || mt == Epi
                then rg
                else G.removeNode gid rg,
             [],
             lns)


-- | Given two typed graph's, return a list of all possible mappings
-- considering only the subgraph inducted by the G.edges.
matchGraphs :: MorphismType -> Graph a b -> Graph a b -> [Mapping]
matchGraphs mt lg rg =
    map (\(m, _, _, _) -> m )
        $ mapGraphs mt lg (([], []), rg, G.edges lg, G.nodes lg)


------------------------------------------------------------------------
-- Isomorphism related (helper) functions

-- | Check if mapping @m@ is surjective. 

-- Currently, @isSurjective@ relies on @nub@ to get the set of G.nodes and G.edges
-- mapped in @m@. The number of elements in this set is compared to those from
-- graph @g@ to see if all got mapped.
-- TODO: use Data.Set for efficiency reasons.
isSurjective :: Graph a b -> Mapping -> Bool
isSurjective g m@(nmaps, emaps) =
    numNodes g == length nmaps && numEdges g == length emaps

-- | Check if a mapping is injective.

-- If the mapping is empty, it's by  definition injective. Otherwise, test, by
-- calling the helper function @iter@ over all node and edge mappings, if they
-- represent an injective relation.  @iter@ does so by checking if any "right
-- side" node/edge got mapped twice, with help of @mem@ that "remembers" the
-- node/G.edges scanned so far.
{-
isInjective :: MapSet -> Bool
isInjective ([], []) = True
isInjective (nms, ems) =
        iter nms [] &&
        iter ems []
        where
                iter xs mem =
                        case xs of
                        [] -> True
                        ((_, t):xs) -> if t `elem` mem
                                          then False
                                          else iter xs (t:mem)        
-}

-- | List all isomorphisms (represented as mappings) between both graphs.
findIsoMorphisms :: Graph a b -> Graph a b -> [Mapping]
findIsoMorphisms lg rg
    | numNodes lg /= numNodes rg ||
      numEdges lg /= numEdges rg = []
    | otherwise = findMatches Iso lg rg

{-
        else filter isInjective $
                         filter (isSurjective g) $
                                 findMatches Iso l g
-}

-- | Check if there's an isomorphism between two graphs.
isIsomorphic :: Graph a b -> Graph a b -> Bool
isIsomorphic a b = findIsoMorphisms a b /= []

----------------------------------------------
-- Helper functions

numNodes :: Graph a b -> Int
numNodes g = length $ G.nodes g

numEdges :: Graph a b -> Int
numEdges g = length $ G.edges g

nullG :: Graph a b -> Bool
nullG g = null (G.nodes g) && null (G.edges g)
