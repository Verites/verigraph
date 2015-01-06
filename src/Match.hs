{-# LANGUAGE TypeFamilies #-}
module Match
    (
{-
    findMatches
    , findMatchesR
    , isSurjective
    --, isInjective
    , findIsoMorphisms
    , isIsomorphic
    , MorphismType (..)
-}
    )
    where

import Control.Monad (foldM)
import Control.Monad.State
import qualified Data.List as L
import Data.List.Utils
import Data.Maybe
import Graph (Graph, edges, Edge, EdgeId, Node, NodeId, nodes, null, sourceOf, targetOf)
import GraphRule (GraphRule)
import qualified Data.Set as Set
import Morphism (domain, codomain, epimorphism)
import GraphMorphism (GraphMorphism, TypedGraph)
import qualified GraphMorphism as GM
import TypedGraphMorphism (mapping, typedMorphism, TypedGraphMorphism)

-- | Is a tuple of two relations regarding two graphs (possibly equal):
-- the first among their respective G.nodes, the other among their G.edges. Each
-- relation is described as a list of (Int, Int) tuples.

data MorphismType = Normal | Mono | Epi | Iso 
    deriving (Eq)

unsafeSourceOf :: Graph a b -> EdgeId -> NodeId
unsafeSourceOf g = head . sourceOf g

unsafeTargetOf :: Graph a b -> EdgeId -> NodeId
unsafeTargetOf g = head . sourceOf g


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
        MatchState rule mt l r (edges lg) (nodes lg) $
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
    getEdges        :: [EdgeId], -- edges to match
    getNodes        :: [NodeId], -- nodes to match
    getMatch        :: TypedGraphMorphism a b
    }

matchGraphs :: (Eq a, Eq b)
            => MatchState a b
            -> [MatchState a b]
matchGraphs st@(MatchState _ mt _ r [] [] m) =
    case mt of
    Epi | epimorphism m -> [st]
        | otherwise     -> []
    Iso | GM.null r  -> [st]
        | otherwise     -> []
    otherwise -> [st]
    
matchGraphs st@(MatchState rule mt l r (le:les) lns m) =
    matchAllEdges le candidates >>= matchGraphs
  where
    lg = domain l
    rg = domain r
    candidates = filter (matchesSameSource st le) $
                 filter (matchesSameTarget st le) $
                 querySameTypeEdges st le
    matchEdges le re =
        MatchState rule mt l r les lns $
           typedMorphism (domain m) (codomain m) $
               GM.updateEdges le re $
               GM.updateNodes (unsafeSourceOf lg le) (unsafeSourceOf rg re) $
               GM.updateNodes (unsafeTargetOf lg le) (unsafeTargetOf rg re) $
               mapping m
    matchAllEdges le res =
        map (matchEdges le) res
        

querySameTypeEdges :: MatchState a b -> EdgeId -> [EdgeId]
querySameTypeEdges st eid =
    GM.applyEdge rinv typeId
  where
    l = getLTypedGraph st
    r = getRTypedGraph st
    rinv = GM.inverse r
    [typeId] = GM.applyEdge l eid

-- | First check if @le@'s source already occurs in the current mapping.
-- If that's the case, check if @re@'s source is the same node to which @le@'s
-- source got mapped.  If so, @re@ is a matching Edge. If @le@'s source doesn't
-- occur in the current mapping, any @re@ will satisfy this condition.

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

-- | First check if @le@'s target already occurs in the current mapping.
-- If that's the case, check if @re@'s target is the same node to which @le@'s
-- target got mapped.  If so, @re@ is a matching Edge. If @le@'s target doesn't
-- occur in the current mapping, any @re@ will satisfy this condition.

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


          
    


{-
-- | Generate an edge condition that checks if both G.edges are from same type.
-- edgeTypeCondGen :: Int -> TypedGraph a b -> TypedGraph a b -> EdgeCondition b
--
-- edgeTypeCondGen le lg rg = (\ge -> sameEdgeType le lg re rg)

-- | Generate a condition that tests if @le@'s source already occurs in @m@.
-- If that's the case, check if @ge@'s source is the same node to which @le@'s
-- source got mapped.  If so, @ge@ is a matching Edge. If @le@'s source doesn't
-- occur in @m@, any @ge@ will satisfy this condition.

srcIdCondGen :: TypedMorphism a b -> TypedGraph a b -> Edge a b -> TypedGraph a b -> EdgeCondition a b
srcIdCondGen m lg le rg =
    (\ge ->
        let res :: [Bool]
            res =
             do lsrc   <- G.sourceOf (M.domain lg) le
                gsrc   <- G.sourceOf (M.domain rg) ge
                rnode  <- M.GM.applyNode m lsrc
                return $ gsrc == rnode
        in null res || head res)
          
         

{-
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
tarIdCondGen :: Morphism a b -> TypedGraph a b -> Int -> TypedGraph a b -> EdgeCondition b
tarIdCondGen m l le g =
    (\ge -> let ltar   = G.targetOf le l
                gtar   = G.targetOf ge g
                mapped = Morph.applyToNode ltar m
            in case mapped of        
                [rnode]   -> gtar == [rnode]
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
-- that check node coincidence (like @srcIdCond@) rely on previously mappings,
-- so they aren't able to detect a mapping node in the current step. 
-- Without @loopCond@, a loop edge that, e.g., happens to be the first to be
-- mapped passes srcIdCond and tarIdCond.
loopCondGen :: TypedGraph a b -> Int -> TypedGraph a b -> EdgeCondition b
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
        :: Int
        -> TypedGraph a b
        -> TypedGraph a b
        -> Morphism a b
        -> [EdgeCondition b]
generateEdgeConds le lg rg m =
    edgeTypeCondGen le l g :
    --srcTypeCondGen l le g :
    --tarTypeCondGen l le g :
    srcIdCondGen m le lg rg :
    tarIdCondGen m le lg rg :
    loopCondGen le lg rg  :
    []


-- | If all conditions in @cl@ get satisfied by the given edge @ge@, return the
-- @m@ mapping with the new edge/G.nodes added. 
processEdge :: [EdgeCondition b] -> Int -> TypedGraph a b -> Bool
processEdge cl e rg = foldr (\c acc -> (c e) && acc) True cl 

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
nodeTypeCondGen :: Int -> TypedGraph a b -> TypedGraph a b -> NodeCondition a
nodeTypeCondGen ln lg rg = (\gn -> sameNodeType ln lg rn rg)

{-
-- | If @ln@ is marked to be deleted, create a NodeCondition that checks if
-- @gn@ has an edge pointed from/at it.
danglingCondGen ::
        Morphism a b
        -> TypedGraph a b
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
    -> Int
    -> TypedGraph a b
    -> TypedGraph a b
    -> Morphism a b
    -> [NodeCondition a]
generateConds r ln lg rg m =
    nodeTypeCondGen ln lg rg :
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
    -> TypedGraph a b        -- ^ @l@, the "left side" graph
    -> (Morphism a b, TypedGraph a b, [Int], [Int]) -- ^ @m@, what already got mapped
    -> [(Morphism a b, TypedGraph a b, [Int], [Int])]
-- When everything got mapped.
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
mapGraphs rule mt lg (m, rg, (le:les), lns) =
    let conds = generateEdgeConds le lg rg m
        edgeList = G.edges g
        candidates = filter (processEdge conds g) $ edgeList
        newMorphism a bs = fmap (processEdgeCandidate g) candidates
    in newMorphism a bs >>= mapGraphs r mt l
      where
        -- adds edge candidate to a mapping
        processEdgeCandidate g ge =
            let res = do
                gs <- G.sourceOf ge g
                gt <- G.targetOf ge g
                ls <- G.sourceOf le l
                lt <- G.targetOf le l
                let newLNodeList = filter (\nid -> (Just nid /= G.sourceOf le l) && (Just nid /= G.targetOf le l)) lns
                return (
                    ((\nm -> addToAL nm ls gs) $
                     (\nm -> addToAL nm lt gt) $
                     nmap,
                     addToAL emap le ge),
                    if mt == Normal || mt == Epi
                       then g
                       else G.removeNode gs $ G.removeNode gt $ G.removeEdge ge g,
                    les,
                    newLNodeList)
            in fromJust res
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
matchGraphs :: Morphism a b -> MorphismType -> TypedGraph a b -> TypedGraph a b -> [Morphism a b]
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
isSurjective :: TypedGraph a b -> Morphism a b -> Bool
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

----------------------------------------------
-- Helper functions

sameNodeType :: Int -> TypedGraph a b -> Int TypedGraph a b -> Bool
sameNodeType ln l gn g =
    Morph.applyToNode ln l == Morph.applyToNode gn g

sameEdgeType :: Int -> TypedGraph a b -> Int -> TypedGraph a b -> Bool
sameEdgeType le l ge g =
    Morph.applyToEdge le l == Morph.applyToEdge ge g

numNodes :: TypedGraph a b -> Int
numNodes g = length $ G.nodes $ Morph.domain g

numEdges :: TypedGraph a b -> Int
numEdges g = length $ G.edges $ Morph.domain g

nullG :: TypedGraph a b -> Bool
nullG g = (null $ G.nodes $ Morph.domain g) && (null $ G.edges $ Morph.domain g)
-}
-}
