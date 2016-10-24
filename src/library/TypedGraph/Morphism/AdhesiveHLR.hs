{-# OPTIONS_GHC -Wno-orphans #-}
module TypedGraph.Morphism.AdhesiveHLR where

import           Abstract.Morphism
import           Abstract.AdhesiveHLR
import           Graph.Graph              as G
import qualified Graph.GraphMorphism      as GM
import           TypedGraph.Graph
import           TypedGraph.Morphism.Core

import           Data.List                                       as L
import           Data.Maybe


instance AdhesiveHLR (TypedGraphMorphism a b) where
  -- @
  --        d
  --    B──────▶C
  --    │       │
  --  b │  (1)  │ c
  --    ▼       ▼
  --    A──────▶A'
  --        f
  -- @
  --
  -- This function receives f, creates the morphism b,
  -- and executes the pushout complement on (1).
  --
  -- The initial pushout operation for typed graphs constructs a graph
  -- with the minimal elements needed in B such that there exists the
  -- pushout (1).
  -- The existence of them ensures that gluing conditions are satisfied.
  -- Two kinds of elements must be created: the nodes that potencially
  -- arises a dangling condition; and the elements that potencially
  -- arises an identification condition.
  calculateInitialPushout f = (b,d,c)
    where
      -- 1. It defines b initially with an empty morphism to A, and then
      -- dangling nodes, collapsed nodes and collapsed edges are added.
      -- d and c are the pushout complement of f and b.
      b = addCollapsedEdges $ addCollapsedNodes $ addDanglingNodes init
        where
          init = emptyMorphismToA
          addDanglingNodes m = addNodes m danglingNodes
          addCollapsedNodes m = addNodes m collapsedNodes
          addCollapsedEdges m = addEdges m collapsedEdges

      (d,c) = calculatePushoutComplement f b

      -- 2. It just defines the names for the structures
      typeGraph = codomain typedGraphA
      typedGraphA = domain f
      nodeTypesInA = GM.applyNodeUnsafe typedGraphA
      edgeTypesInA = GM.applyEdgeUnsafe typedGraphA
      graphA = domain typedGraphA
      graphA' = domain (codomain f)
      edgesOfA = edgesFromDomain f
      nodesOfA = nodesFromDomain f

      emptyMorphismToA = buildTypedGraphMorphism emptyTypedGraph typedGraphA emptyMapToA
        where
          emptyTypedGraph = GM.empty empty typeGraph
          emptyMapToA = GM.empty empty graphA


      -- 3. Auxiliary functions

      -- It captures all nodes in A that when mapped to A' has an incident edge.
      danglingNodes = filter checkExistsOrphanIncidentEdge nodesOfA
        where
          checkExistsOrphanIncidentEdge n = any (isOrphanEdge f) incEdges
            where
              incEdges = incidentEdges graphA' (applyNodeUnsafe f n)

      -- It captures all nodes in A that are equally mapped by f
      collapsedNodes =
        filter
          (\n ->
            any
              (\n' ->
                n/=n' &&
                (applyNodeUnsafe f n == applyNodeUnsafe f n')
              ) nodesOfA
          ) nodesOfA

      -- It captures all edges in A that are equally mapped by f
      collapsedEdges =
        concatMap
          (\e ->
            [(e, sourceOfUnsafe graphA e, targetOfUnsafe graphA e) |
              any  (\ e' -> e /= e' && (applyEdgeUnsafe f e == applyEdgeUnsafe f e')) edgesOfA]
          ) edgesOfA

      -- It adds a list of nodes in a morphism
      addNodes = foldr (\n -> createNodeOnDomain n (nodeTypesInA n) n)

      -- It adds a list of edges (with source and target nodes) in a morphism
      addEdges =
        foldr
          (\(e,src,tgt) b ->
            (createEdgeOnDomain e src tgt (edgeTypesInA e) e
              (createNodeOnDomain tgt (nodeTypesInA tgt) tgt
                (createNodeOnDomain src (nodeTypesInA src) src b)
              )
            )
          )

  {-
          g
      A──────▶C
      │       │
     f│   =   │g'
      ▼       ▼
      B──────▶D
          g'

     PO algorithm:
     1. invert r
     2. compose k and r^-1
     3. create node table  (C -> D)
     5. create edge table  (C -> D)
     4. associate nodes
     6. associate edges
  -}
  calculatePushout f g =
    let
        gf = compose (invert g) f                                 -- invert g and compose with f, obtain gf : C -> B
        cOnlyNodes = orphanTypedNodes g                           -- nodes in C that are not mapped from g
        cOnlyEdges = orphanTypedEdges g                           -- edges in C that are not mapped from g
        nodesInF'  = zip cOnlyNodes (newTypedNodes $ codomain gf) -- table mapping NodeIds in C to NodeIds in D
        edgesInF'  = zip cOnlyEdges (newTypedEdges $ codomain gf) -- table mapping EdgeIds in C to EdgeIds in D

        -- generate new node instances in D, associating them to the "created" nodes in C
        gf' = generateNewNodeInstances gf nodesInF'

        -- query the instance graphs C
        typemor = domain         gf'                     -- typemor is the typed graph (C -> T)
        d       = domain         typemor                 -- d  is the instance graph C
        mp      = mapping        gf'                     -- mp is the mapping of gf'  : (C -> B'), where B' = B + new nodes
        s1 e = fromJust $ G.sourceOf d e                 -- obtain source of e in C
        t1 e = fromJust $ G.targetOf d e                 -- obtain target of e in C
        s2 e = fromJust $ GM.applyNode mp (s1 e)         -- obtain source of m'(e) in D
        t2 e = fromJust $ GM.applyNode mp (t1 e)         -- obtain target of m'(e) in D
        tp e = fromJust $ GM.applyEdge typemor e         -- obtain type of e in C

        -- generate new edge table with new information
        edgeTable' = map (\(e,e2) -> (e, s1 e, t1 e, e2, s2 e, t2 e, tp e)) edgesInF'

        -- create new morphism adding all edges
        kr''      = generateNewEdgeInstances gf' edgeTable'
    in (kr'', idMap (codomain f) (codomain kr''))


  {-
     PO complement algorithm:
     1. compose l and m generating ml
     2. query edges for deletion in the codomain of ml
     2. query nodes for deletion in the codomain of ml
     3. delete all edges
     4. delete all nodes
  -}
  calculatePushoutComplement m l =
    let ml       = compose l m                                                         -- compose l and m obtaining ml
        delEdges = mapMaybe (GM.applyEdge $ mapping m) (orphanTypedEdges l) -- obtain list of edges to be deleted in G
        delNodes = mapMaybe (GM.applyNode $ mapping m) (orphanTypedNodes l) -- obtain list of nodes to be deleted in G
        k        = foldr removeNodeFromCodomain                                          -- delete all edges, then all nodes from ml
                       (foldr removeEdgeFromCodomain ml delEdges)
                           delNodes
    in (k, idMap (codomain k) (codomain m))


  monomorphicPullback f g = (delNodesFromF', delNodesFromG')
    where
      f' = invert f
      g' = invert g
      nodes = nodesFromDomain f'
      edges = edgesFromDomain f'
      knodes = filter (\n -> isJust (applyNode f' n) && isJust (applyNode g' n)) nodes
      kedges = filter (\e -> isJust (applyEdge f' e) && isJust (applyEdge g' e)) edges
      delNodes = nodes \\ knodes
      delEdges = edges \\ kedges
      delEdgesFromF' = foldr removeEdgeFromDomain f' delEdges
      delNodesFromF' = foldr removeNodeFromDomain delEdgesFromF' delNodes
      delEdgesFromG' = foldr removeEdgeFromDomain g' delEdges
      delNodesFromG' = foldr removeNodeFromDomain delEdgesFromG' delNodes


  hasPushoutComplement (Monomorphism, g) (_, f) =
    satisfiesDanglingCondition f g


  hasPushoutComplement (_, g) (_, f) =
    satisfiesDanglingCondition f g && satisfiesIdentificationCondition f g


generateNewNodeInstances :: TypedGraphMorphism a b -> [(NodeId, NodeId)] -> TypedGraphMorphism a b
generateNewNodeInstances gf =
  foldr (\(a,b) tgm -> let tp = fromJust $ GM.applyNode (domain gf) a
                       in updateNodeRelation a b tp tgm) gf


generateNewEdgeInstances :: TypedGraphMorphism a b -> [(EdgeId, NodeId, NodeId, EdgeId, NodeId, NodeId, EdgeId)]
  -> TypedGraphMorphism a b
generateNewEdgeInstances =
  foldr (\(a,_,_,b,sb,tb,tp) tgm -> updateEdgeRelation a b (createEdgeOnCodomain b sb tb tp tgm) )



---- Gluing Conditions

-- | Return True if the match @m@ satifies the identification condition for existence of
-- a pushout complement
satisfiesIdentificationCondition :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
satisfiesIdentificationCondition l m =
  all (==True) (notIdentificatedNodes ++ notIdentificatedEdges)
  where
    notIdentificatedNodes =
      map (notIdentificatedElement l m nodesFromDomain applyNode) (nodesFromCodomain m)
    notIdentificatedEdges =
      map (notIdentificatedElement l m edgesFromDomain applyEdge) (edgesFromCodomain m)


-- | Given a left-hand-side morphism /l : K -> L/, a match /m : L -> G/, two functions /domain/ (to get all elements
-- in the domain of m) and /apply/ (for applying an element in a TypedGraphMorphism),
-- and an element __/e/__ (that can be either a __/Node/__ or an __/Edge/__) it returns true if /e/ is
-- identificated (i.e. __e__ has more than one incident element on itself and at least one of them deletes it)
notIdentificatedElement :: Eq t => TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b -> [t])
                         -> (TypedGraphMorphism a b -> t -> Maybe t) -> t -> Bool
notIdentificatedElement l m domain apply e = (length incidentElements <= 1) || not eIsDeleted
  where
    incidentElements = [a | a <- domain m, apply m a == Just e]
    l' = apply (invert l)
    eIsDeleted = Nothing `elem` map l' incidentElements


-- | Given a left-hand-side morphism /l : K -> L/, a match /m : L -> G/, returns @true@ if
-- there aren't dangling edges
satisfiesDanglingCondition :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
satisfiesDanglingCondition l m = all (==True) (concat incidentDeletedEdges)
    where
        lhs = graphDomain m
        instanceGraph = graphCodomain m
        checkEdgeDeletion = map (checkDeletion l m applyEdge edgesFromDomain)
        matchedNodes = mapMaybe (applyNode m) (nodes lhs)
        deletedNodes = filter (checkDeletion l m applyNode nodesFromDomain) matchedNodes
        incidentEdgesOnDeletedNodes = map (incidentEdges instanceGraph) deletedNodes
        incidentDeletedEdges = map checkEdgeDeletion incidentEdgesOnDeletedNodes


-- | TODO: Find a better name for this function, that was repeated both here and in the GraphRule archive
-- | Given the left-hand-side morphism of a rule /l : K -> L/, a match /m : L -> G/ for this rule, an element __/e/__
-- (that can be either a __/Node/__ or an __/Edge/__) and two functions /apply/ (for applying that element in a TypedGraphMorphism) and
-- /list/ (to get all the corresponding elements in the domain of m), it returns true if /e/ is deleted by this rule for the given match
checkDeletion :: Eq t => TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b -> t -> Maybe t)
          -> (TypedGraphMorphism a b -> [t]) -> t -> Bool
checkDeletion l m apply list e = elementInL && not elementInK
  where
    elementInL = any (\x -> apply m x == Just e) (list m)
    kToG = compose l m
    elementInK = any (\x -> apply kToG x == Just e) (list kToG)

isOrphanEdge :: TypedGraphMorphism a b -> EdgeId -> Bool
isOrphanEdge m n = n `elem` orphanTypedEdges m
