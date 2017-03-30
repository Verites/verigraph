module TypedGraph.Morphism.AdhesiveHLR where

import           Abstract.AdhesiveHLR
import           Abstract.Morphism
import           Graph.Graph                    as G
import qualified Graph.GraphMorphism            as GM
import           TypedGraph.Morphism.Cocomplete ()
import           TypedGraph.Morphism.Core

import           Data.Maybe                     (fromJust, fromMaybe, mapMaybe)


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
              incEdges = getIncidentEdges graphA' (applyNodeUnsafe f n)

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
     PO complement algorithm:
     1. compose l and m generating ml
     2. query edges for deletion in the codomain of ml
     2. query nodes for deletion in the codomain of ml
     3. delete all edges
     4. delete all nodes
  -}
  calculatePushoutComplement m l =
    let ml       = compose l m                                              -- compose l and m obtaining ml
        delEdges = mapMaybe (GM.applyEdge $ mapping m) (orphanTypedEdges l) -- obtain list of edges to be deleted in G
        delNodes = mapMaybe (GM.applyNode $ mapping m) (orphanTypedNodes l) -- obtain list of nodes to be deleted in G
        k        = foldr removeNodeFromCodomain                             -- delete all edges, then all nodes from ml
                       (foldr removeEdgeFromCodomain ml delEdges)
                           delNodes
    in (k, idMap (codomain k) (codomain m))

  -- @
  --        g'
  --     X──────▶A
  --     │       │
  --  f' │       │ f
  --     ▼       ▼
  --     B──────▶C
  --        g
  --
  -- @
  --
  -- Pullback for typed graphs.
  -- It starts getting all pairs for nodes and edges, this pairs will be
  -- the elements of X.
  -- It creates an empty X, and morphisms f' and g', and adds each pair
  -- identifing them with ids.
  calculatePullback f g = (f'',g'')
    where
      -- This first part just defines the names for the structures
      nodeTypeInB = GM.applyNodeUnsafe typedGraphB
      nodeTypeInA = GM.applyNodeUnsafe typedGraphA
      edgeTypeInB = GM.applyEdgeUnsafe typedGraphB
      edgeTypeInA = GM.applyEdgeUnsafe typedGraphA
      typeGraph = codomain typedGraphC
      typedGraphA = domain f
      typedGraphB = domain g
      typedGraphC = codomain f
      graphB = domain typedGraphB
      graphA = domain typedGraphA

      nodesInA = nodesFromDomain f
      nodesInB = nodesFromDomain g
      edgesInA = edgesFromDomain f
      edgesInB = edgesFromDomain g

      -- Discover the nodes and edges of the X
      nodesWithoutId = getPairs applyNodeUnsafe nodesInA nodesInB nodeIds
      nodesWithId = zip nodesWithoutId ([0..]::[Int])

      egdesWithoutId = getPairs applyEdgeUnsafe edgesInA edgesInB edgeIds
      edgesWithId = zip egdesWithoutId ([0..]::[Int])

      -- Run the product for all elements that are mapped on the same element in C
      getPairs apply elemA elemB list = concatMap (uncurry product) comb
        where
          comb =
            map
              (\n ->
                (filter (\n' -> apply f n' == n) elemA,
                 filter (\n' -> apply g n' == n) elemB))
              (list (domain typedGraphC))

          product x y = [(a,b) | a <- x, b <- y]

      -- Init X, f' and g' as empty
      initX = GM.empty empty typeGraph
      initF' = buildTypedGraphMorphism initX typedGraphB (GM.empty empty (domain typedGraphB))
      initG' = buildTypedGraphMorphism initX typedGraphA (GM.empty empty (domain typedGraphA))

      -- Add all elements on X and their morphisms
      (g',f') = foldr updateNodes (initG',initF') nodesWithId
      (g'',f'') = foldr updateEdges (g',f') edgesWithId

      -- Add a node is just do it on the domain of f' and g'
      updateNodes ((a,b),newId) (g',f') = (updateG',updateF')
        where
          newNode = NodeId newId
          updateG' = createNodeOnDomain newNode (nodeTypeInA a) a g'
          updateF' = createNodeOnDomain newNode (nodeTypeInB b) b f'

      -- Add an edge on the domain of f' and g'
      updateEdges ((a,b),newId) (g',f') = (updateG',updateF')
        where
          newEdge = EdgeId newId

          -- To add an edge, their source and target nodes must be found (they already exists).
          -- It searches the node in X that is mapped by f' and g' to the same source (resp. target) node of the related edges on A and B.
          sourceOfUnsafe g e = fromMaybe (error "sourceOf error") $ sourceOf g e
          src1 =
            filter
              (\n ->
                applyNodeUnsafe f' n == sourceOfUnsafe graphB b &&
                applyNodeUnsafe g' n == sourceOfUnsafe graphA a)
              (nodesFromDomain f')
          src = if Prelude.null src1 then error "src not found" else head src1

          targetOfUnsafe g e = fromMaybe (error "targetOf error") $ targetOf g e
          tgt1 =
            filter
              (\n ->
                applyNodeUnsafe f' n == targetOfUnsafe graphB b &&
                applyNodeUnsafe g' n == targetOfUnsafe graphA a)
              (nodesFromDomain f')
          tgt = if Prelude.null tgt1 then error "tgt not found" else head tgt1

          updateG' = createEdgeOnDomain newEdge src tgt (edgeTypeInA a) a g'
          updateF' = createEdgeOnDomain newEdge src tgt (edgeTypeInB b) b f'


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
        matchedNodes = mapMaybe (applyNode m) (nodeIds lhs)
        deletedNodes = filter (checkDeletion l m applyNode nodesFromDomain) matchedNodes
        incidentEdgesOnDeletedNodes = map (getIncidentEdges instanceGraph) deletedNodes
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
