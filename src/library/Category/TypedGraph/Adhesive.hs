module Category.TypedGraph.Adhesive (isDeleted) where

import           Data.Maybe                         (fromJust, mapMaybe)

import           Abstract.Category
import           Abstract.Category.Adhesive
import           Category.TypedGraph.Category
import           Category.TypedGraph.Limit          ()
import           Category.TypedGraph.Finitary       ()
import           Data.Graphs                        as G
import qualified Data.Graphs.Morphism               as GM
import           Data.TypedGraph.Morphism


instance MInitialPushout (TypedGraphMorphism a b) where
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
  calculateMInitialPushout f = (b,d,c)
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

      (d,c) = calculatePushoutComplementAlongM b f

      -- 2. It just defines the names for the structures
      typeGraph = codomain typedGraphA
      typedGraphA = domain f
      nodeTypesInA = GM.applyNodeIdUnsafe typedGraphA
      edgeTypesInA = GM.applyEdgeIdUnsafe typedGraphA
      graphA = domain typedGraphA
      graphA' = domain (codomain f)
      edgesOfA = edgesFromDomain f
      nodeIdsOfA = nodeIdsFromDomain f

      emptyMorphismToA = buildTypedGraphMorphism emptyTypedGraph typedGraphA emptyMapToA
        where
          emptyTypedGraph = GM.empty empty typeGraph
          emptyMapToA = GM.empty empty graphA


      -- 3. Auxiliary functions

      -- It captures all nodes in A that when mapped to A' has an incident edge.
      danglingNodes = filter checkExistsOrphanIncidentEdge nodeIdsOfA
        where
          checkExistsOrphanIncidentEdge n = any (\(_,e,_) -> isOrphanEdge f (edgeId e)) incEdges
            where
              Just (_,ctx) = lookupNodeInContext (applyNodeIdUnsafe f n) graphA'
              incEdges = incidentEdges ctx

      -- It captures all nodes in A that are equally mapped by f
      collapsedNodes =
        filter
          (\n ->
            any
              (\n' ->
                n/=n' &&
                (applyNodeIdUnsafe f n == applyNodeIdUnsafe f n')
              ) nodeIdsOfA
          ) nodeIdsOfA

      -- It captures all edges in A that are equally mapped by f
      collapsedEdges =
        concatMap
          (\e ->
            [(edgeId e, sourceId e, targetId e) |
              any  (\e' -> edgeId e /= edgeId e' && (applyEdgeIdUnsafe f (edgeId e) == applyEdgeIdUnsafe f (edgeId e'))) edgesOfA]
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

instance MAdhesive (TypedGraphMorphism a b) where

  {-
     PO complement algorithm:
     1. compose l and m generating ml
     2. query edges for deletion in the codomain of ml
     2. query nodes for deletion in the codomain of ml
     3. delete all edges
     4. delete all nodes
  -}
  calculatePushoutComplementAlongM l m =
    let ml       = m <&> l                                                -- compose l and m obtaining ml
        delEdges = mapMaybe (GM.applyEdgeId $ mapping m) (orphanTypedEdgeIds l) -- obtain list of edges to be deleted in G
        delNodes = mapMaybe (GM.applyNodeId $ mapping m) (orphanTypedNodeIds l) -- obtain list of nodes to be deleted in G
        k        = foldr removeNodeFromCodomain                               -- delete all edges, then all nodes from ml
                       (foldr removeEdgeFromCodomain ml delEdges)
                           delNodes
    in (k, idMap (codomain k) (codomain m))

  hasPushoutComplementAlongM l m =
    satisfiesDanglingCondition l m && satisfiesIdentificationCondition l m

-- TODO: it looks like the function below shouldn't be in the module
generateNewNodeInstances :: TypedGraphMorphism a b -> [(NodeId, NodeId)] -> TypedGraphMorphism a b
generateNewNodeInstances gf =
  foldr (\(a,b) tgm -> let tp = fromJust $ GM.applyNodeId (domain gf) a
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
      map (notIdentificatedElement l m nodeIdsFromDomain applyNodeId) (nodeIdsFromCodomain m)
    notIdentificatedEdges =
      map (notIdentificatedElement l m edgeIdsFromDomain applyEdgeId) (edgeIdsFromCodomain m)


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


-- | Given a left-hand-side morphism /l : K -> L/, a match /m : L -> G/,
-- returns @true@ if there are not dangling edges
satisfiesDanglingCondition :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
satisfiesDanglingCondition l m = Prelude.null incidentEdgesNotDeleted
  where
    lhs = graphDomain m
    instanceGraph = graphCodomain m

    deletedNodes =
      [(n',ctx) |
         (n',ctx) <- nodesInContext instanceGraph,
         any (\n -> applyNodeIdUnsafe m n == nodeId n') (nodeIds lhs),
         isDeleted l m applyNodeId nodeIdsFromDomain (nodeId n')]

    incidentEdgesNotDeleted =
      [edgeId e |
         ((n1,_),e,(n2,_)) <- edgesInContext instanceGraph,
         (n,_) <- deletedNodes,
         nodeId n `elem` [nodeId n1, nodeId n2],
         not (isDeleted l m applyEdgeId edgeIdsFromDomain (edgeId e))]

-- TODO: this function should not be here in this module
-- | Given the left-hand-side morphism of a rule /l : K -> L/, a match /m : L -> G/ of this rule, an element __/e/__
-- (that can be either a __/Node/__ or an __/Edge/__) and two functions /apply/ (for applying that element in a TypedGraphMorphism) and
-- /list/ (to get all the corresponding elements in the domain of m), it returns true if /e/ is deleted by this rule for the given match
isDeleted :: Eq t => TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b -> t -> Maybe t)
          -> (TypedGraphMorphism a b -> [t]) -> t -> Bool
isDeleted l m apply list e = elementInL && not elementInK
  where
    elementInL = any (\x -> apply m x == Just e) (list m)
    kToG = m <&> l
    elementInK = any (\x -> apply kToG x == Just e) (list kToG)

-- TODO: this function should not be here in this module
isOrphanEdge :: TypedGraphMorphism a b -> EdgeId -> Bool
isOrphanEdge m n = n `elem` orphanTypedEdgeIds m

instance FinalPullbackComplement (TypedGraphMorphism a b) where
  
    -- @
    --       l
    --    K──────▶L
    --    │       V
    --  k │  (1)  │ m
    --    ▼       ▼
    --    D──────▶A
    --       l'
    -- @
    --
    -- This function receives m and l, it creates (k,l') as the
    -- the final pullback complement on (1).
    --
    -- __morphism m must be injective__
    --
    -- The algorithm follows Construction 6 of Sesqui-pushout rewriting.
    -- Available on:
    -- http://www.ti.inf.uni-due.de/publications/koenig/icgt06b.pdf
    --
    -- It is a naive implementation focused on correction and not performance.
    -- Performance may be reasonable for epi pairs rewrite, but poor when large contexts.
    --
    -- The resulting graph D contains a copy of K, a copy of the largest
    -- subgraph of A which is not in the image of m, and a suitable number
    -- of copies of each edge of A incident to a node in m(l(K)):
    -- this has the effect of "cloning" part of A.
    --
    -- This function is divided in four steps,
    -- first two for nodes and the lasts for edges.
    calculateFinalPullbackComplement m l = step4
      where
        typedGraphK = domain l
        typedGraphA = codomain m
        graphK = domain typedGraphK
        graphA = domain typedGraphA
        edgeTypeInK = GM.applyEdgeIdUnsafe typedGraphK
        edgeTypeInA = GM.applyEdgeIdUnsafe typedGraphA
        nodeTypeInK = GM.applyNodeIdUnsafe typedGraphK
        nodeTypeInA = GM.applyNodeIdUnsafe typedGraphA
        typeGraph = codomain typedGraphK
  
        -- Inits (k:K->D, l':D->A) with D as empty.
        initD = GM.empty empty typeGraph
        initK = buildTypedGraphMorphism typedGraphK initD (GM.empty graphK empty)
        initL' = buildTypedGraphMorphism initD typedGraphA (GM.empty empty graphA)
  
        -- Step1 adds in D a copy of the nodes of K.
        step1 = foldr updateNodesFromK (initK,initL') nodesAddFromK
        nodesAddFromK = zip (nodeIdsFromDomain l) ([0..]::[Int])
        updateNodesFromK (n,newId) (k,l') = (updatedK2,updatedL')
          where
            newNode = NodeId newId
            typeN = nodeTypeInK n
            appliedL = applyNodeIdUnsafe l n
            appliedA = applyNodeIdUnsafe m appliedL
            updatedK = createNodeOnCodomain newNode typeN k
            updatedK2 = untypedUpdateNodeRelation n newNode updatedK
            updatedL' = createNodeOnDomain newNode typeN appliedA l'
  
        -- Step2 adds in D the nodes out of the image of m.
        step2 = foldr updateNodesFromA step1 nodesAddFromMatch
        nodesAddFromMatch = zip (orphanTypedNodeIds m) ([(length nodesAddFromK)..]::[Int])
        updateNodesFromA (n,newId) (k,l') = (updatedK,updatedL')
          where
            newNode = NodeId newId
            typeN = nodeTypeInA n
            updatedK = createNodeOnCodomain newNode typeN k
            updatedL' = createNodeOnDomain newNode typeN n l'
  
        -- Step3 adds in D a copy of the edges of K.
        step3@(_,edgesL') = foldr updateEdgesFromK step2 edgesAddFromK
        edgesAddFromK = zip (edgesFromDomain l) ([0..]::[Int])
        updateEdgesFromK (e,newId) (k,l') = (updatedK2,updatedL')
          where
            newEdge = EdgeId newId
            appliedL = applyEdgeIdUnsafe l (edgeId e)
            appliedA = applyEdgeIdUnsafe m appliedL
            typeE = edgeTypeInK (edgeId e)
            src = applyNodeIdUnsafe k (sourceId e)
            tgt = applyNodeIdUnsafe k (targetId e)
            updatedK = createEdgeOnCodomain newEdge src tgt typeE k
            updatedK2 = updateEdgeRelation (edgeId e) newEdge updatedK
            updatedL' = createEdgeOnDomain newEdge src tgt typeE appliedA l'
  
        -- Step4 adds in D a replication of edges out of the image of m,
        -- where source and target nodes may have been cloned in D.
        step4 = foldr updateEdgesFromA step3 edgesAddFromMatch
        edgesAddFromMatch = zip edgesFromA ([(length edgesAddFromK)..]::[Int])
          where
            edgesFromA = [(edgeId e, u, v) |
                           e <- orphanTypedEdges m,
                           u <- nodeIdsFromDomain edgesL',
                           v <- nodeIdsFromDomain edgesL',
                           sourceId e == applyNodeIdUnsafe edgesL' u,
                           targetId e == applyNodeIdUnsafe edgesL' v]
        updateEdgesFromA ((e,u,v),newId) (k,l') = (updatedK,updatedL')
          where
            newEdge = EdgeId newId
            typeE = edgeTypeInA e
            updatedK = createEdgeOnCodomain newEdge u v typeE k
            updatedL' = createEdgeOnDomain newEdge u v typeE e l'
  
    hasFinalPullbackComplement (cls, _) _ 
      | cls `isSubclassOf` monic = True
      | otherwise = error "Final pullback complement is not implemented for non monomorphic matches"
  