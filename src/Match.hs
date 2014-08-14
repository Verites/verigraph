module Graph.Match
    (
    Mapping
    , findMatches
    , findMatchesR
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
findMatches mt l g = 
    findMatchesR (Morph.empty l g) mt l g

-- | Given two typed graphs, return a list of mappings, each representing a
-- possible homomorphism between the graphs.
findMatchesR :: Morphism a b -> MorphismType -> Graph a b -> Graph a b -> [Mapping]
findMatchesR r mt l g = matchGraphs r mt l g

----------------------------------------------------------------------------
-- Edge related condition functions

-- An EdgeCondition checks if a node satisfies it's internal requirements.
type EdgeCondition b = Int -> Bool

-- | Generate an edge condition that checks if both G.edges are from same type.
edgeTypeCondGen :: Graph a b -> Int -> Graph a b -> EdgeCondition b
edgeTypeCondGen l le g = (\ge -> sameEdgeType l le g ge)

-- | Generate a condition that tests if @le@'s source already occurs in @m@.
-- If that's the case, check if @ge@'s source is the same node to which @le@'s
-- source got mapped.  If so, @ge@ is a matching Edge. If @le@'s source doesn't
-- occur in @m@, any @ge@ will satisfy this condition.
srcIDCondGen :: Graph a b -> Int -> Mapping -> Graph a b -> EdgeCondition b
srcIDCondGen l le m@(nmatches, _) g =
    (\ge ->
        let lsrc    = G.sourceOf le l
            gsrc    = G.sourceOf ge g
            matched = find (\(s, t) -> Just s == lsrc) nmatches
        in case matched of        
            Just (_, n) -> gsrc == Just n
            otherwise -> True)

{-
                        matched = Set.toList $ Set.filter (\(s, t) -> s == lsrc) Set.toList nmatches
                in case matched of
                        ((_, n):ns) -> gsrc == n
                        otherwise   -> True)
-}

-- | Generate an edge condition that tests if @le@'s target already occurs in
-- @m@. If that's the case, check if @ge@'s target is the same node to which
-- @le@'s target got mapped.  If so, @ge@ is a matching Edge. If @le@'s target
-- doesn't occur in @m@, any @ge@ will satisfy this condition.
tarIDCondGen :: Graph a b -> Int -> Mapping -> Graph a b -> EdgeCondition b
tarIDCondGen l le m@(nmatches, _) g =
    (\ge -> let ltar    = G.targetOf le l
                gtar    = G.targetOf ge g
                matched = find (\(s, t) -> Just s == ltar) nmatches
            in case matched of        
                Just (_, n) -> gtar == Just n
                otherwise -> True)
{-
                    matched = Set.toList $ Set.filter (\(s, t) -> s == ltar) nmatches
                in case matched of
                        ((_, n):ns) -> gtar == n
                        otherwise   -> True)
-}

-- | Generate an edge condition that checks If @le@ is a loop edge, check if
-- @ge@ is also a loop in @g@.

-- This condition is due to the sequential nature of processEdges. Conditions 
-- that check node coincidence (like @srcIDCond@) rely on previously mappings,
-- so they aren't able to detect a mapping node in the current step. 
-- Without @loopCond@, a loop edge that, e.g., happens to be the first to be
-- mapped passes srcIDCond and tarIDCond.
loopCondGen :: Graph a b -> Int -> Graph a b -> EdgeCondition b
loopCondGen l le g =
    (\ge -> let lsrc = G.sourceOf le l
                ltar = G.targetOf le l
                gsrc = G.sourceOf ge g
                gtar = G.targetOf ge g
            in if lsrc == ltar
                then gsrc == gtar
                else True)

-- | Feed each generator it's parameters and collect the resulting
-- nodeconditions in a list.
generateEdgeConds
        :: Graph a b
        -> Int
        -> Graph a b
        -> Mapping
        -> [EdgeCondition b]
generateEdgeConds l le g m =
    edgeTypeCondGen l le g :
    --srcTypeCondGen l le g :
    --tarTypeCondGen l le g :
    srcIDCondGen l le m g :
    tarIDCondGen l le m g :
    loopCondGen l le g  :
    []


-- | If all conditions in @cl@ get satisfied by the given edge @ge@, return the
-- @m@ mapping with the new edge/G.nodes added. 
processEdge :: [EdgeCondition b] -> Graph a b -> Int -> Bool
processEdge cl g e = foldr (\c acc -> (c e) && acc) True cl 

-------------------------------------------------------------------------
-- Node related condition functions

-- A NodeCondition checks if a node satisfies it's internal requirements.
type NodeCondition a = Int -> Bool

-- Condition Generators
-- This functions are meant to be called from another one that has access to a 
-- global picture from the graph morphism structures (Morphism, L graph, G Graph,
-- node from L being mapped etc.). They generate NodeConditions to be applied
-- to G.nodes from the G graph.

-- | Generate a node condition that checks if both G.nodes are from same type.
nodeTypeCondGen :: Graph a b -> Int -> Graph a b -> NodeCondition a
nodeTypeCondGen l ln g = (\gn -> sameNodeType l ln g gn)

{-
-- | If @ln@ is marked to be deleted, create a NodeCondition that checks if
-- @gn@ has an edge pointed from/at it.
danglingCondGen ::
        Morphism a b
        -> Graph a b
        -> Int
        -> NodeCondition a
danglingCondGen r g ln
    | toBeDeleted r ln = (\gn -> not $ hasEdge g gn)
    | otherwise        = (\gn -> True)
      where
        hasEdge g gn = not $ null $ incidentEdges gn g


-- | Generate a node condition that first checks if @gn@ was already mapped. If
-- so, let it pass. Otherwise, check if both @ln@ and @gn@ are to be deleted. 
-- If so, they can be mapped to each other.
delCondGen ::
        Morphism a b
        -> Int
        -> MapSet
        -> NodeCondition a
delCondGen r ln m =
    (\gn -> (not $ isMapped gn m) ||
            (toBeDeleted r ln == mappedToDel r m gn))

-- | Check if @gn@ is a right-side node in the given mapping.
isMapped :: Int -> MapSet -> Bool
isMapped gn (nmaps, _) =
    let found = Set.filter (\(_, gnode) -> gnode == gn) nmaps
    in not $ Set.null found
                
-- | Check if @n@ was mapped to a L node marked to be deleted.
mappedToDel :: Morphism a b -> MapSet -> Int -> Bool
mappedToDel r (nmaps, _) n =
    let nmap = Set.filter (\(lnid, gnid) ->
                    gnid == n && toBeDeleted r lnid
                    ) nmaps
    in not $ Set.null nmap

-- | Check if the L node with id @nid@ is marked to be deleted.
toBeDeleted :: Morphism a b -> Int -> Bool
toBeDeleted r nid =
    let naction =
         find (\na -> case na of
                    (Just ln, _) -> ln == nid
                    otherwise    -> False)
                $ nodeActions r
    in case naction of
        Just (_, Nothing) -> True
        otherwise         -> False
-}
                
-- | Feed each generator it's parameters and collect the resulting
-- nodeconditions in a list.
generateConds ::
    Morphism a b
    -> Graph a b
    -> Int
    -> Graph a b
    -> Mapping
    -> [NodeCondition a]
generateConds r l ln g m =
    nodeTypeCondGen l ln g :
--    delCondGen r ln m      :
--    danglingCondGen r g ln :
    []

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
    Morphism a b
    -> MorphismType
    -> Graph a b        -- ^ @l@, the "left side" graph
    -> (Mapping, Graph a b, [Int], [Int]) -- ^ @m@, what already got mapped
    -> [(Mapping, Graph a b, [Int], [Int])]
mapGraphs _ mt _ ml@((nmap, emap), g, [], []) =
    case mt of
    Epi ->
        let gMappedNodes = foldr (\(ln, gn) acc -> addToAL acc ln gn) [] nmap
            gMappedEdges = foldr (\(le, ge) acc -> addToAL acc le ge) [] emap
        in  if length gMappedNodes == numNodes g && length gMappedEdges == numEdges g
               then [ml]
               else []
    Iso -> if nullG g
              then [ml]
              else []
    otherwise -> [ml]
mapGraphs r mt l (m@(nmatch, ematch), g, (le:les), lns) =
    let conds = generateEdgeConds l le g m
        edgeList = G.edges g
        candidates = filter (processEdge conds g) $ edgeList
        newMapSets = fmap (processEdgeCandidate g) candidates
    in newMapSets >>= mapGraphs r mt l
      where
        processEdgeCandidate g ge =
            let sid = fromJust $ G.sourceOf ge g --FIX: should avoid fromJust
                tid = fromJust $ G.targetOf ge g
                newLNodeList = filter (\nid ->
                    (Just nid /= G.sourceOf le l) && (Just nid /= G.targetOf le l)
                    ) lns
            in (((\nm -> addToAL nm (fromJust $ G.sourceOf le l) sid) $
                 (\nm -> addToAL nm (fromJust $ G.targetOf le l) tid) $
                 nmatch,
                 addToAL ematch le ge),
                if mt == Normal || mt == Epi
                    then g
                    else G.removeNode sid $ G.removeNode tid $ G.removeEdge ge g,
                les,
                newLNodeList)
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


-- | Given two typed graph's, return a list of all possible mappings
-- considering only the subgraph inducted by the G.edges.
matchGraphs :: Morphism a b -> MorphismType -> Graph a b -> Graph a b -> [Mapping]
matchGraphs r mt l g =
    map (\(m, _, _, _) -> m )
        $ mapGraphs r mt l (([], []), g, G.edges l, G.nodes l)


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
isIsomorphic :: Graph a b -> Graph a b -> Bool
isIsomorphic a b = findIsoMorphisms a b /= []

----------------------------------------------
-- Helper functions

sameNodeType :: Graph a b -> Int -> Graph a b -> Int -> Bool
sameNodeType l ln g gn =
    G.getTypeOfNode ln l == G.getTypeOfNode gn l

sameEdgeType :: Graph a b -> Int -> Graph a b -> Int -> Bool
sameEdgeType l le g ge =
    G.getTypeOfEdge le l == G.getTypeOfEdge ge g

numNodes :: Graph a b -> Int
numNodes g = length $ G.nodes g

numEdges :: Graph a b -> Int
numEdges g = length $ G.edges g

nullG :: Graph a b -> Bool
nullG g = null (G.nodes g) && null (G.edges g)
